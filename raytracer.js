"use strict";

var cameraPosition = [ 0.0, 0.0, -1.0 ];
var wallOffset = 0.8;
var spherePositions = [
    [ 0.8, 0.0, 2.7 ],
    [ 0.0, -0.3, 2.9 ],
    [ -0.3, 0.0, 2.3 ],
    ];
var sphereRadiuses = [
    0.3,
    0.4,
    0.17,
    ];

//// Render view
const vsSource =
	`#version 300 es

	precision highp float;

	in vec4 vertexPosition;
	in vec3 vertexTextureCoord;

	out vec3 vTextureCoord;

	void main(void) {
		gl_Position = vertexPosition;
		vTextureCoord = vertexTextureCoord;
	}
`;
// Fragment shader program
const fsSource =
	`#version 300 es
	#define M_PI 3.1415926535897932384626433832795
	#define NUM_SPHERE 16

	precision highp float;

	uniform sampler2D texture;
	uniform mat3 textureMatrix;
	uniform vec3 cameraPosition;
	uniform int numberOfSphere;
	uniform vec3 spherePosition[NUM_SPHERE];
	uniform float sphereRadius[NUM_SPHERE];
	//uniform vec3 sphereColor;
	uniform float wallOffset; // the distance from center

	in vec3 vTextureCoord;

	out vec4 fragmentColor;

	float random(vec2 v) {
		return fract(sin(dot(v, vec2(12.9898, 78.233))) * 43758.5453);
	}

	struct TraceData {
		vec3 origin;
		vec3 ray;
		vec3 col;
		vec3 reflection;
	};

	// args:
	//     origin: start point of ray
	//     ray: direction of ray (should be normalized)
	TraceData traceRay(const TraceData traceStart, const float seed) {
		const float d_max = 100000.0;

		////
		// sphere hit
		////
		vec3 spherePosition_min = vec3(0.0);
		vec3 sphereReflectPoint_min = vec3(0.0);
		float r_min_sphere = d_max;
		for (int n = 0; n < numberOfSphere; n++) {
			vec3 sphereVector = spherePosition[n] - traceStart.origin;
			vec3 r_sphereRay = cross(sphereVector, traceStart.ray);
			vec3 sphereReflectPoint =
			    (dot(sphereVector, traceStart.ray) - sqrt(sphereRadius[n] * sphereRadius[n] - r_sphereRay * r_sphereRay))
			    * traceStart.ray + traceStart.origin;
			float r_min = mix(
			    length(sphereReflectPoint - traceStart.origin),
			    d_max,
			    step(0.0, -dot(sphereVector, traceStart.ray) * step(-sphereRadius[n], -length(r_sphereRay))));
			// choose only nearest one
			r_min_sphere = min(r_min, r_min_sphere);
			if (r_min == r_min_sphere) {
				spherePosition_min = spherePosition[n];
				sphereReflectPoint_min = sphereReflectPoint;
			}
			/*
			float cond = step(0.0, r_min_sphere - r_min);
			spherePosition_min = mix(
			    spherePosition_min,
			    spherePosition[n],
			    cond);
			sphereReflectPoint_min = mix(
			    sphereReflectPoint_min,
			    sphereReflectPoint,
			    cond);
			*/
		}

		////
		// wall hit
		////
		float r_top = abs(wallOffset - traceStart.origin.y) / dot(vec3(0.0, 1.0, 0.0), traceStart.ray);
		r_top += (1.0 - step(0.0, r_top)) * d_max;
		float r_bottom = abs(-wallOffset - traceStart.origin.y) / dot(vec3(0.0, -1.0, 0.0), traceStart.ray);
		r_bottom += (1.0 - step(0.0, r_bottom)) * d_max;
		float r_left = abs(-wallOffset - traceStart.origin.x) / dot(vec3(-1.0, 0.0, 0.0), traceStart.ray);
		r_left += (1.0 - step(0.0, r_left)) * d_max;
		float r_right = abs(wallOffset - traceStart.origin.x) / dot(vec3(1.0, 0.0, 0.0), traceStart.ray);
		r_right += (1.0 - step(0.0, r_right)) * d_max;
		float r_min_wall = min(r_top, r_bottom);
		r_min_wall = min(r_left, r_min_wall);
		r_min_wall = min(r_right, r_min_wall);

		////
		// calculate color and reflected ray
		////
		TraceData trace = traceStart;
		if (r_min_sphere <= r_min_wall) {
			vec3 n = normalize(sphereReflectPoint_min - spherePosition_min);
			trace.origin = sphereReflectPoint_min;
			trace.ray = normalize(
			    reflect(traceStart.ray, n)
			    + 0.25 * vec3(
				random(vec2(traceStart.ray.x, seed)),
				random(vec2(traceStart.ray.y, seed)),
				random(vec2(traceStart.ray.z, seed)))
			    );
			trace.col += vec3(0.0, 0.0, 0.0) * traceStart.reflection;
			trace.reflection *= vec3(0.4 + 0.5 * length(cross(trace.ray, n)));
		} else {
			trace.origin = vec3(0.0);
			trace.ray = vec3(0.0);
			vec3 col_wall = vec3(0.0);
			vec3 normal = vec3(0.0);
			float reflection = 0.0;
			// Top
			float cond = step(0.0, r_min_wall - r_top);
			col_wall += cond * vec3(0.9, 0.0, 0.0);
			trace.ray += cond * reflect(traceStart.ray, vec3(0.0, -1.0, 0.0));
			normal += cond * vec3(0.0, -1.0, 0.0);
			reflection += cond * 0.8;
			// Bottom
			cond = step(0.0, r_min_wall - r_bottom);
			//col_wall += cond * vec3(0.9, 0.9, 0.0);
			trace.ray += cond * reflect(traceStart.ray, vec3(0.0, 1.0, 0.0));
			normal += cond * vec3(0.0, 1.0, 0.0);
			reflection += cond * 0.3;
			// Left
			cond = step(0.0, r_min_wall - r_left);
			col_wall += cond * vec3(0.0, 0.9, 0.0);
			trace.ray += cond * reflect(traceStart.ray, vec3(1.0, 0.0, 0.0));
			normal += cond * vec3(1.0, 0.0, 0.0);
			reflection += cond * 0.8;
			// Right
			cond = step(0.0, r_min_wall - r_right);
			col_wall += cond * vec3(0.0, 0.0, 0.9);
			trace.ray += cond * reflect(traceStart.ray, vec3(-1.0, 0.0, 0.0));
			normal += cond * vec3(-1.0, 0.0, 0.0);
			reflection += cond * 0.8;
			// result
			trace.origin = traceStart.ray * r_min_wall + traceStart.origin;
			trace.ray = normalize(
			    trace.ray
			    + 0.0625 * vec3(
				random(vec2(traceStart.ray.x, seed)),
				random(vec2(traceStart.ray.y, seed)),
				random(vec2(traceStart.ray.z, seed)))
			    );
			trace.col += col_wall * traceStart.reflection;
			trace.reflection *= reflection * vec3(0.5 + 0.5 * pow(length(cross(trace.ray, normal)), 50.0));
		}

		// saturation of color
		trace.col = min(trace.col, 1.0);
		return trace;
	}

	void main(void) {
		const float max_iter = 64.0;
		const int max_bound = 4;

		vec3 color = vec3(0.0);
		for (float s = 0.0; s < max_iter; s += 1.0) {
			TraceData trace;
			trace.origin = cameraPosition;
			trace.ray = normalize(vTextureCoord);
			trace.col = vec3(0.0);
			trace.reflection = vec3(1.0);
			for (int i = 0; i < max_bound; i++) {
				trace = traceRay(trace, s);
				// Offset for eliminate reflection on same surface
				trace.origin += trace.ray * 0.00001;
			}
			color += trace.col / max_iter;
		}
		fragmentColor = vec4(color, 1.0);
	}
`;



window.onload = main;

function main() {
	const canvas = document.querySelector('#glcanvas');

	// Get WebGL instance
	const gl = canvas.getContext('webgl2', { antialias: true });
	// If we don't have a GL context, give up now
	if (!gl) {
		alert('Unable to initialize WebGL. Your browser or machine may not support it.');
		return;
	}

	// Initialize a shader program; this is where all the lighting
	// for the vertices and so forth is established.
	const renderShaderProgram = initShaderProgram(gl, vsSource, fsSource);

	// Collect all the info needed to use the shader program.
	// Look up which attributes our shader program is using
	// for aVertexPosition, aVevrtexColor and also
	// look up uniform locations.
	const renderProgramInfo = {
		shaderProgram: renderShaderProgram,
		attribLocations: {
			vertexPosition: gl.getAttribLocation(renderShaderProgram, 'vertexPosition'),
			vertexTextureCoord: gl.getAttribLocation(renderShaderProgram, 'vertexTextureCoord'),
		},
		uniformLocations: {
			texture: gl.getUniformLocation(renderShaderProgram, 'texture'),
			textureMatrix: gl.getUniformLocation(renderShaderProgram, 'textureMatrix'),
			cameraPosition: gl.getUniformLocation(renderShaderProgram, 'cameraPosition'),
			wallOffset: gl.getUniformLocation(renderShaderProgram, 'wallOffset'),
			numberOfSphere: gl.getUniformLocation(renderShaderProgram, 'numberOfSphere'),
			spherePosition: spherePositions.map((x, ind) => {
				return gl.getUniformLocation(renderShaderProgram, 'spherePosition[' + ind + ']');
			    }),
			sphereRadius: sphereRadiuses.map((x, ind) => {
				return gl.getUniformLocation(renderShaderProgram, 'sphereRadius[' + ind + ']');
			    }),
		},
	};

	const screen = createScreen();
	const screenBuffers = createVBO(gl, screen);
	const textures = createTexture(gl);

	// Draw the scene repeatedly
	let count = 0;
	function render(now) {
		cameraPosition[0] = Math.sin(2.0 * Math.PI * count / 300) * 0.1;
		//spherePositions[0][0] = Math.cos(2.0 * Math.PI * count / 217) * 0.9;
		spherePositions[0][1] = Math.sin(2.0 * Math.PI * count / 217) * 0.2;
		++count;
		drawScene(gl, renderProgramInfo, textures, screenBuffers);

		requestAnimationFrame(render);
	}
	requestAnimationFrame(render);
}


function createScreen() {
	const positions = [
		-1.0, -1.0,  0.0,
		 1.0, -1.0,  0.0,
		 1.0,  1.0,  0.0,
		-1.0,  1.0,  0.0,
	];

	const indices = [
		0, 1, 2, 0, 2, 3,
	];

	const texcoords = [
		-1.0, -1.0, 1.0,
		 1.0, -1.0, 1.0,
		 1.0,  1.0, 1.0,
		-1.0,  1.0, 1.0,
	];

	return {
		positions: positions,
		texcoords: texcoords,
		indices: indices,
	};
}

function createVBO(gl, data) {
	// Bind buffers
	let positionBuffer = gl.createBuffer();
	gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer);
	gl.bufferData(
			gl.ARRAY_BUFFER,
			new Float32Array(data.positions),
			gl.STATIC_DRAW);

	let texcoordBuffer = gl.createBuffer();
	gl.bindBuffer(gl.ARRAY_BUFFER, texcoordBuffer);
	gl.bufferData(
		gl.ARRAY_BUFFER,
		new Float32Array(data.texcoords),
		gl.STATIC_DRAW);

	let indexBuffer = gl.createBuffer();
	gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, indexBuffer);
	gl.bufferData(
		gl.ELEMENT_ARRAY_BUFFER,
		new Uint16Array(data.indices),
		gl.STATIC_DRAW);

	return {
		positions: {
			data: positionBuffer,
			numComponents: 3,
			type: gl.FLOAT,
			normalize: false,
		},
		texcoords: {
			data: texcoordBuffer,
			numComponents: 3,
			type: gl.FLOAT,
			normalize: false,
		},
		indices: indexBuffer,
		vertexCount: data.indices.length,
	};
}

function createTexture(gl) {
	//const img = document.getElementById("textureImage");

	const tx = gl.createTexture();
	gl.bindTexture(gl.TEXTURE_2D, tx);
	//gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, gl.RGBA, gl.UNSIGNED_BYTE, img);
	gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, 1024, 1024, 0, gl.RGBA, gl.UNSIGNED_BYTE, null);
	gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
	gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
	gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR);
	gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR);

	return [tx];
}

function initShaderProgram(gl, vsSource, fsSource) {
	let vertexShader = loadShader(gl, gl.VERTEX_SHADER, vsSource);
	let fragmentShader = loadShader(gl, gl.FRAGMENT_SHADER, fsSource);

	let shaderProgram = gl.createProgram();
	gl.attachShader(shaderProgram, vertexShader);
	gl.attachShader(shaderProgram, fragmentShader);
	gl.linkProgram(shaderProgram);

	if (!gl.getProgramParameter(shaderProgram, gl.LINK_STATUS)) {
		alert("Unable to initialize the shader program: " + gl.getProgramInfoLog(shaderProgram));
		return null;
	}
	return shaderProgram;
}

function loadShader(gl, type, source) {
	let shader = gl.createShader(type);
	gl.shaderSource(shader, source);
	gl.compileShader(shader);
	if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
		alert("Failed to compile the shader program: " + gl.getShaderInfoLog(shader));
		gl.deleteShader(shader);
		return null;
	}
	return shader;
}

function drawScene(gl, renderProgramInfo, textures, screenBuffers)
{
	////////////////////////////////////////////////////////////////
	// Render
	////////////////////////////////////////////////////////////////
	gl.useProgram(renderProgramInfo.shaderProgram);

	gl.clearColor(0.0, 0.0, 0.0, 1.0); // Clear with black
	gl.clearDepth(1.0); // Clear everything
	gl.clear(gl.COLOR_BUFFER_BIT | gl.DETPH_BUFFER_BIT);

	gl.activeTexture(gl.TEXTURE0);
	gl.bindTexture(gl.TEXTURE_2D, textures[0]);

	const textureMatrix = createIdenticalMat3();
	textureMatrix[0] = 0.5; textureMatrix[4] = -0.5; // Scale
	textureMatrix[6] = 0.5; textureMatrix[7] = 0.5; // Shift

	gl.uniform1i(
	    renderProgramInfo.uniformLocations.texture,
	    0);
	gl.uniformMatrix3fv(
	    renderProgramInfo.uniformLocations.textureMatrix,
	    false,
	    textureMatrix);
	gl.uniform3fv(
	    renderProgramInfo.uniformLocations.cameraPosition,
	    cameraPosition);
	gl.uniform1f(
	    renderProgramInfo.uniformLocations.wallOffset,
	    wallOffset);
	gl.uniform1i(
	    renderProgramInfo.uniformLocations.numberOfSphere,
	    Math.min(spherePositions.length, sphereRadiuses.length));
	for (let i = 0; i < spherePositions.length; i++) {
		gl.uniform3fv(
		    renderProgramInfo.uniformLocations.spherePosition[i],
		    spherePositions[i]);
		gl.uniform1f(
		    renderProgramInfo.uniformLocations.sphereRadius[i],
		    sphereRadiuses[i]);
	}

	enableAttribute(gl, renderProgramInfo.attribLocations, screenBuffers);
	gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, screenBuffers.indices);
	gl.drawElements(gl.TRIANGLES, screenBuffers.vertexCount, gl.UNSIGNED_SHORT, 0);
}

function enableAttribute(gl, locations, buffers) {
	let stride = 0;
	let offset = 0;
	// Positions
	{
		gl.bindBuffer(gl.ARRAY_BUFFER, buffers.positions.data);
		gl.vertexAttribPointer(
			locations.vertexPosition,
			buffers.positions.numComponents,
			buffers.positions.type,
			buffers.positions.normalize,
			stride,
			offset);
		gl.enableVertexAttribArray(locations.vertexPosition);
	}
	// Texture Coords
	{
		gl.bindBuffer(gl.ARRAY_BUFFER, buffers.texcoords.data);
		gl.vertexAttribPointer(
			locations.vertexTextureCoord,
			buffers.texcoords.numComponents,
			buffers.texcoords.type,
			buffers.texcoords.normalize,
			stride,
			offset);
		gl.enableVertexAttribArray(locations.vertexTextureCoord);
	}
}

function createIdenticalMat3() {
	let A = new Array(9);
	A[0] = 1.0; A[3] = 0.0; A[6] = 0.0;
	A[1] = 0.0; A[4] = 1.0; A[7] = 0.0;
	A[2] = 0.0; A[5] = 0.0; A[8] = 1.0;
	return A;
}

function createIdenticalMat4() {
	let A = new Array(16);
	A[0] = 1.0; A[4] = 0.0; A[8]  = 0.0; A[12] = 0.0;
	A[1] = 0.0; A[5] = 1.0; A[9]  = 0.0; A[13] = 0.0;
	A[2] = 0.0; A[6] = 0.0; A[10] = 1.0; A[14] = 0.0;
	A[3] = 0.0; A[7] = 0.0; A[11] = 0.0; A[15] = 1.0;
	return A;
}

