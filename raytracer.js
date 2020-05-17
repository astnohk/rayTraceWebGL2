"use strict";

var cameraPosition = [ 0.0, 0.0, -1.0 ];
var wallOffset = 0.8;
var spherePositions = [
    [ 0.8, 0.0, 2.9 ],
    [ 0.0, -0.3, 2.9 ],
    [ -0.3, 0.0, 2.3 ],
    ];
var sphereRadiuses = [
    0.3,
    0.4,
    0.17,
    ];

var max_iter = 32.0;
var max_bound = 8;


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

	uniform float seed;
	uniform sampler2D texture;
	uniform mat3 textureMatrix;
	uniform vec3 cameraPosition;
	uniform int numberOfSphere;
	uniform vec3 spherePosition[NUM_SPHERE];
	uniform float sphereRadius[NUM_SPHERE];
	//uniform vec3 sphereColor;
	uniform float wallOffset; // the distance from center
	uniform float max_iter;
	uniform int max_bound;

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
	TraceData traceRay(const TraceData traceStart, const float iter) {
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
			    (dot(sphereVector, traceStart.ray) - sqrt(pow(sphereRadius[n], 2.0) - pow(length(r_sphereRay), 2.0)))
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
		float r_random = 0.5 * random(vec2(min(r_min_sphere, r_min_wall), seed + iter));
		if (iter > max_iter * 0.8 && r_random < min(r_min_sphere, r_min_wall)) {
			// Particle
			trace.origin = r_random * traceStart.ray + traceStart.origin;
			trace.ray = normalize(
			    traceStart.ray + 0.5 * normalize(vec3(
				    random(vec2(traceStart.ray.x, seed + iter)),
				    random(vec2(traceStart.ray.y, seed + iter)),
				    random(vec2(traceStart.ray.z, seed + iter))
			    ))
			    );
			trace.reflection *= 0.9 * vec3(
			    0.5 + 0.5 * dot(traceStart.ray, trace.ray),
			    1.0,
			    0.5 + 0.5 * length(cross(traceStart.ray, trace.ray)));
		} else {
			if (r_min_sphere <= r_min_wall) {
				vec3 n = normalize(sphereReflectPoint_min - spherePosition_min);
				trace.origin = sphereReflectPoint_min;
				trace.ray = normalize(mix(
				    reflect(traceStart.ray, n) + 0.00625 * normalize(vec3(
					random(vec2(traceStart.ray.x, seed + iter)),
					random(vec2(traceStart.ray.y, seed + iter)),
					random(vec2(traceStart.ray.z, seed + iter))
				    )),
				    n + 0.9 * normalize(vec3(
					random(vec2(traceStart.ray.x, seed + iter)),
					random(vec2(traceStart.ray.y, seed + iter)),
					random(vec2(traceStart.ray.z, seed + iter))
				    )),
				    step(1.0, iter)));
				trace.col += vec3(0.0, 0.0, 0.0) * traceStart.reflection;
				float f0 = 0.6;
				trace.reflection *= vec3(0.8, 0.9, 1.2)
				    * vec3(mix(
					f0 + (1.0 - f0) * pow(1.0 - dot(trace.ray, n), 5.0),
					f0 * dot(trace.ray, n),
					step(1.0, iter)));
			} else {
				trace.origin = vec3(0.0);
				trace.ray = vec3(0.0);
				vec3 col_wall = vec3(0.0);
				vec3 incidentRay = vec3(0.0);
				vec3 normal = vec3(0.0);
				vec3 reflection = vec3(0.0);
				// Top
				float cond = step(0.0, r_min_wall - r_top);
				col_wall += cond * vec3(1.8);
				incidentRay += cond * traceStart.ray;
				normal += cond * vec3(0.0, -1.0, 0.0);
				reflection += cond * vec3(0.6);
				// Bottom
				cond = step(0.0, r_min_wall - r_bottom);
				col_wall += cond * vec3(0.0);
				incidentRay += cond * traceStart.ray;
				normal += cond * vec3(0.0, 1.0, 0.0);
				reflection += cond * vec3(0.2);
				// Left
				cond = step(0.0, r_min_wall - r_left);
				col_wall += cond * 0.8 * vec3(0.1, 1.0, 0.1);
				incidentRay += cond * traceStart.ray;
				normal += cond * vec3(1.0, 0.0, 0.0);
				reflection += cond * vec3(0.1, 1.0, 0.1);
				// Right
				cond = step(0.0, r_min_wall - r_right);
				col_wall += cond * 0.5 * vec3(0.2, 0.2, 1.0);
				incidentRay += cond * traceStart.ray;
				normal += cond * vec3(-1.0, 0.0, 0.0);
				reflection += cond * vec3(0.3, 0.3, 1.0);
				// result
				normal = normalize(normal);
				trace.origin = traceStart.ray * r_min_wall + traceStart.origin;
				trace.ray = normalize(mix(
				    reflect(incidentRay, normal) + 0.00625 * normalize(vec3(
					random(vec2(traceStart.ray.x, seed + iter)),
					random(vec2(traceStart.ray.y, seed + iter)),
					random(vec2(traceStart.ray.z, seed + iter))
				    )),
				    normal + 0.9 * normalize(vec3(
					random(vec2(traceStart.ray.x, seed + iter)),
					random(vec2(traceStart.ray.y, seed + iter)),
					random(vec2(traceStart.ray.z, seed + iter))
				    )),
				    step(1.0, iter)));
				trace.col += col_wall * traceStart.reflection;
				float f0 = 0.8;
				trace.reflection *=
				    reflection * vec3(mix(
					f0 + (1.0 - f0) * pow(1.0 - dot(trace.ray, normal), 5.0),
					f0 * dot(trace.ray, normal),
					step(1.0, iter)));
			}
		}
		trace.col = clamp(trace.col, 0.0, 2.0);
		return trace;
	}

	void main(void) {
		vec3 color = vec3(0.0);
		const float diffuse = 0.4;
		for (float iter = 0.0; iter < max_iter; iter += 1.0) {
			TraceData trace;
			trace.origin = cameraPosition;
			trace.ray = normalize(vTextureCoord);
			trace.col = vec3(0.0);
			trace.reflection = vec3(1.0);
			for (int i = 0; i < max_bound; i++) {
				trace = traceRay(trace, iter);
				// Offset for eliminate reflection on same surface
				trace.origin += trace.ray * 0.00001;
			}
			color += trace.col * mix(1.0, mix(1.0 - diffuse, diffuse / (max_iter - 1.0), step(1.0, iter)), step(2.0, max_iter));
		}
		fragmentColor = vec4(clamp(color, 0.0, 1.0), 1.0);
	}
`;



window.onload = init;

function init() {
	// Initialize HTML
	// max iteration count
	const max_iterInput = document.getElementById("maxIterationCount");
	max_iterInput.value = max_iter;
	max_iterInput.addEventListener(
	    "change",
	    function () {
		    max_iter = parseFloat(max_iterInput.value);
		    if (max_iter < 1.0) {
			    max_iter = 1.0;
		    }
	    });
	const max_iterInputIncrement = document.getElementById("maxIterationCountInc");
	const max_iterIncrementer = () => { max_iter = parseFloat(max_iterInput.value) + 1.0; max_iterInput.value = max_iter; };
	max_iterInputIncrement.addEventListener("mousedown", max_iterIncrementer);
	max_iterInputIncrement.addEventListener("touchstart", max_iterIncrementer);
	const max_iterInputDecrement = document.getElementById("maxIterationCountDec");
	const max_iterDecrementer = () => { max_iter = Math.max(parseFloat(max_iterInput.value) - 1.0, 1.0); max_iterInput.value = max_iter; };
	max_iterInputDecrement.addEventListener("mousedown", max_iterDecrementer);
	max_iterInputDecrement.addEventListener("touchstart", max_iterDecrementer);
	// max bound count
	const max_boundInput = document.getElementById("maxBoundCount");
	max_boundInput.value = max_bound;
	max_boundInput.addEventListener(
	    "change",
	    function () {
		    max_bound = parseInt(max_boundInput.value, 10);
		    if (max_bound <= 0) {
			    max_bound = 1;
		    }
	    });
	const max_boundInputIncrement = document.getElementById("maxBoundCountInc");
	const max_boundIncrementer = () => { max_bound = parseInt(max_boundInput.value, 10) + 1; max_boundInput.value = max_bound; };
	max_boundInputIncrement.addEventListener("mousedown", max_boundIncrementer);
	max_boundInputIncrement.addEventListener("touchstart", max_boundIncrementer);
	const max_boundInputDecrement = document.getElementById("maxBoundCountDec");
	const max_boundDecrementer = () => { max_bound = Math.max(parseInt(max_boundInput.value, 10) - 1, 1); max_boundInput.value = max_bound; };
	max_boundInputDecrement.addEventListener("mousedown", max_boundDecrementer);
	max_boundInputDecrement.addEventListener("touchstart", max_boundDecrementer);

	// Start WebGL
	glmain();
}

function glmain() {
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
			seed: gl.getUniformLocation(renderShaderProgram, 'seed'),
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
			max_iter: gl.getUniformLocation(renderShaderProgram, 'max_iter'),
			max_bound: gl.getUniformLocation(renderShaderProgram, 'max_bound'),
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
		sphereRadiuses[2] = 0.17 +
		    0.025 * Math.pow(Math.sin(2.0 * Math.PI * count / 211), 512.0) +
		    0.025 * Math.pow(Math.sin(2.0 * Math.PI * (count + 7) / 211), 512.0);
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

	gl.uniform1f(
	    renderProgramInfo.uniformLocations.seed,
	    Math.random() * 10.0);
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
	gl.uniform1f(
	    renderProgramInfo.uniformLocations.max_iter,
	    max_iter);
	gl.uniform1i(
	    renderProgramInfo.uniformLocations.max_bound,
	    max_bound);
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

