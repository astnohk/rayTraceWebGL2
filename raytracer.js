"use strict";

var cameraPosition = [ 0.0, 0.0, -1.0 ];
var wallOffset = 1.0;
var spherePositions = [
    [ 0.8, 0.0, 2.7 ],
    ];
var sphereRadiuses = [
    0.3,
    ];

//// Render view
const vsSource =
	`#version 300 es

	precision highp float;

	in vec4 vertexPosition;
	in vec3 vertexTextureCoord;

	out vec4 vPosition;
	out vec3 vTextureCoord;

	void main(void) {
		vPosition = vertexPosition;
		gl_Position = vPosition;
		vTextureCoord = vertexTextureCoord;
	}
`;
// Fragment shader program
const fsSource =
	`#version 300 es
	#define M_PI 3.1415926535897932384626433832795

	precision highp float;

	uniform sampler2D texture;
	uniform mat3 textureMatrix;
	uniform vec3 cameraPosition;
	uniform vec3 spherePosition;
	uniform float sphereRadius;
	uniform float wallOffset; // the distance from center

	in vec4 vPosition;
	in vec3 vTextureCoord;

	out vec4 fragmentColor;

	struct TraceData {
		vec3 origin;
		vec3 ray;
		vec3 col;
		vec3 reflection;
	};

	// args:
	//     origin: start point of ray
	//     ray: direction of ray (should be normalized)
	TraceData traceRay(const TraceData traceStart) {
		TraceData trace = traceStart;
		vec3 sphereVector = spherePosition - traceStart.origin;
		vec3 r = cross(sphereVector, traceStart.ray);
		if (dot(sphereVector, traceStart.ray) * step(-sphereRadius, -length(r)) > 0.0) {
			// sphere hit
			trace.origin = (dot(sphereVector, traceStart.ray) - sqrt(sphereRadius * sphereRadius - r * r)) * traceStart.ray + traceStart.origin;
			vec3 n = normalize(trace.origin - spherePosition);
			trace.ray = reflect(traceStart.ray, n);
			trace.col += vec3(0.0, 0.0, 0.0) * traceStart.reflection;
			trace.reflection *= vec3(0.4 + 0.5 * length(cross(trace.ray, n)));
		} else {
			// wall hit
			float r_top = abs(wallOffset - traceStart.origin.y) / dot(vec3(0.0, 1.0, 0.0), traceStart.ray);
			r_top += (1.0 - step(0.0, r_top)) * 10000.0;
			float r_bottom = abs(-wallOffset - traceStart.origin.y) / dot(vec3(0.0, -1.0, 0.0), traceStart.ray);
			r_bottom += (1.0 - step(0.0, r_bottom)) * 10000.0;
			float r_left = abs(-wallOffset - traceStart.origin.x) / dot(vec3(-1.0, 0.0, 0.0), traceStart.ray);
			r_left += (1.0 - step(0.0, r_left)) * 10000.0;
			float r_right = abs(wallOffset - traceStart.origin.x) / dot(vec3(1.0, 0.0, 0.0), traceStart.ray);
			r_right += (1.0 - step(0.0, r_right)) * 10000.0;
			float r_min = min(r_top, r_bottom);
			r_min = min(r_left, r_min);
			r_min = min(r_right, r_min);

			trace.origin = vec3(0.0);
			trace.ray = vec3(0.0);
			vec3 col_wall = vec3(0.0);
			// Top
			col_wall += step(0.0, r_min - r_top) * vec3(1.0, 0.0, 0.0);
			trace.ray += step(0.0, r_min - r_top) * reflect(traceStart.ray, vec3(0.0, -1.0, 0.0));
			// Bottom
			col_wall += step(0.0, r_min - r_bottom) * vec3(1.0, 1.0, 0.0);
			trace.ray += step(0.0, r_min - r_bottom) * reflect(traceStart.ray, vec3(0.0, 1.0, 0.0));
			// Left
			col_wall += step(0.0, r_min - r_left) * vec3(0.0, 1.0, 0.0);
			trace.ray += step(0.0, r_min - r_left) * reflect(traceStart.ray, vec3(1.0, 0.0, 0.0));
			// Right
			col_wall += step(0.0, r_min - r_right) * vec3(0.0, 0.0, 1.0);
			trace.ray += step(0.0, r_min - r_right) * reflect(traceStart.ray, vec3(-1.0, 0.0, 0.0));
			// result
			trace.ray = normalize(trace.ray);
			trace.origin = traceStart.ray * r_min;
			trace.col += col_wall * traceStart.reflection;
			trace.reflection *= vec3(0.3);
		}
		trace.col = min(trace.col, 1.0);
		return trace;
	}

	void main(void) {
		TraceData trace;
		int max_iter = 2;

		trace.origin = cameraPosition;
		trace.ray = normalize(vTextureCoord);
		trace.col = vec3(0.0);
		trace.reflection = vec3(1.0);
		for (int i = 0; i < max_iter; i++) {
			trace = traceRay(trace);
		}
		fragmentColor = vec4(trace.col, 1.0);
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
			spherePosition: gl.getUniformLocation(renderShaderProgram, 'spherePosition'),
			sphereRadius: gl.getUniformLocation(renderShaderProgram, 'sphereRadius'),
		},
	};

	const screen = createScreen();
	const screenBuffers = createVBO(gl, screen);
	const textures = createTexture(gl);

	// Draw the scene repeatedly
	let count = 0;
	function render(now) {
		cameraPosition[0] = Math.sin(2.0 * Math.PI * count / 300) * 0.1;
		spherePositions[0][1] = Math.sin(2.0 * Math.PI * count / 117) * 0.2;
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
	gl.uniform3fv(
		renderProgramInfo.uniformLocations.spherePosition,
		spherePositions[0]);
	gl.uniform1f(
		renderProgramInfo.uniformLocations.sphereRadius,
		sphereRadiuses[0]);

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

