window.addEventListener("load", async () => {
    try {
	const [proc] = await Scheme.load_main("game.wasm", {
	    user_imports: {
		uint8Array: {
		    new: (length) => new Uint8Array(length),
		    fromArrayBuffer: (buffer) => new Uint8Array(buffer),
		    length: (array) => array.length,
		    ref: (array, index) => array[index],
		    set: (array, index, value) => array[index] = value
		},
		webSocket: {
		    close: (ws) => ws.close(),
		    new(url) {
			ws = new WebSocket(url);
			ws.binaryType = "arraybuffer";
			return ws;
		    },
		    send: (ws, data) => ws.send(data),
		    setOnOpen(ws, f) {
			ws.onopen = (e) => {
			    f();
			};
		    },
		    setOnMessage(ws, f) {
			ws.onmessage = (e) => {
			    f(e.data);
			};
		    },
		    setOnClose(ws, f) {
			ws.onclose = (e) => {
			    f(e.code, e.reason);
			};
		    },
		    setOnError(ws, f) {
			ws.onerror = (e) => f();
		    }
		},
		crypto: {
		    digest: (algorithm, data) => globalThis.crypto.subtle
			.digest(algorithm, data).then((arrBuf) => new Uint8Array(arrBuf)),
		    randomValues(length) {
			const array = new Uint8Array(length);
			globalThis.crypto.getRandomValues(array);
			return array;
		    },
		    generateEd25519KeyPair: () => globalThis.crypto.subtle.generateKey(
			{ name: "Ed25519" },
			true,
			["sign", "verify"]
		    ),
		    keyPairPrivateKey: (keyPair) => keyPair.privateKey,
		    keyPairPublicKey: (keyPair) => keyPair.publicKey,
		    exportKey: (key) => globalThis.crypto.subtle.exportKey("raw", key)
			.then((arrBuf) => new Uint8Array(arrBuf)),
		    importPublicKey: (key) => globalThis.crypto.subtle.importKey(
			"raw",
			key,
			{ name: "Ed25519" },
			true,
			["verify"]
		    ),
		    signEd25519: (data, privateKey) => globalThis.crypto.subtle.sign(
			{ name: "Ed25519" },
			privateKey,
			data
		    ).then((arrBuf) => new Uint8Array(arrBuf)),
		    verifyEd25519: (signature, data, publicKey) => globalThis.crypto.subtle
			.verify(
			    { name: "Ed25519" },
			    publicKey,
			    signature,
			    data
			)
		},
		window: {
		    get: () => window,
		    innerWidth: () => window.innerWidth,
		    innerHeight: () => window.innerHeight,
		    requestAnimationFrame: (f) => window.requestAnimationFrame(f),
		    setTimeout: (f, delay) => window.setTimeout(f, delay)
		},
		storage: {
		    setItem: (key, value) => localStorage.setItem(key, value),
		    getItem: (key) => localStorage.getItem(key)
		},
		console: {
		    log: (string) => console.log(string),
		    split: (string, delim) => string.split(delim),
		},
		document: {
		    current_second() { return Math.floor(Date.now() / 1000); },
		    get: () => document,
		    body: () => document.body,
		    getElementById: (id) => document.getElementById(id),
		    createTextNode: (text) => document.createTextNode(text),
		    createElement: (tag) => document.createElement(tag),
		    addFont: (font) => document.fonts.add(font),
		    createTreeWalker: Document.prototype.createTreeWalker.bind(document),
		    // Fullscreen API
		    requestFullscreen: (element) => {
			try {
			    console.log('Requesting fullscreen for element:', element);
			    return element.requestFullscreen();
			} catch (e) {
			    console.warn('Failed to request fullscreen:', e);
			}
		    },
		    exitFullscreen: () => {
			try {
			    if (document.fullscreenElement) {
				return document.exitFullscreen();
			    }
			} catch (e) {
			    console.warn('Failed to exit fullscreen:', e);
			}
		    },
		    fullscreenElement: () => document.fullscreenElement,
	    isFullscreen: () => document.fullscreenElement !== null ? 1 : 0
		},
		element: {
		    value: (elem) => elem.value,
		    setValue: (elem, value) => elem.value = value,
		    width: (elem) => elem.width,
		    height: (elem) => elem.height,
		    setWidth: (elem, width) => elem.width = width,
		    setHeight: (elem, height) => elem.height = height,
		    appendChild: (parent, child) => parent.appendChild(child),
		    setAttribute: (elem, name, value) => elem.setAttribute(name, value),
		    removeAttribute: (elem, name) => elem.removeAttribute(name),
		    remove: (elem) => elem.remove(),
		    replaceWith: (oldElem, newElem) => oldElem.replaceWith(newElem),
		    clone: (elem) => elem.cloneNode(),
		    then: (promise, func) => promise.then((font) => func(font)),
		    removeEventListener(elem, name, f) { elem.removeEventListener(name, f); },
		    addEventListener(elem, name, f) { elem.addEventListener(name, f); },
		    scrollHeight(elem) { return elem.scrollHeight; },
		    setScrollTop(elem, value) { elem.scrollTop = value; },
		    setChecked(elem, checked) { elem.checked = (checked == 1); },
		    checked(elem) { return elem.checked; },
		},
		event: {
		    addEventListener: (target, type, listener) => target.addEventListener(type, listener),
		    removeEventListener: (target, type, listener) => target.removeEventListener(type, listener),
		    preventDefault: (event) => event.preventDefault(),
		    keyboardCode: (event) => event.code,
		    keyboardKey(event) { return event.key; },
		    keyboardShiftKey(event) { return event.shiftKey; }
		},
		image: {
		    new: (src) => {
			const img = new Image();
			img.src = src;
			return img;
		    }
		},
		media: {
		    load: (font) => font.load(),
		    makeFont: (name, url) => new FontFace(name, url),
		    newAudio: (src) => new Audio(src),
		    play: (media) => media.play(),
		    pause: (media) => media.pause(),
		    volume: (media) => media.volume,
		    setVolume: (media, volume) => media.volume = volume,
		    setLoop: (media, loop) => media.loop = (loop == 1),
		    seek: (media, time) => media.currentTime = time
		},
		canvas: {
		    save: (ctx) => ctx.save(),
		    restore: (ctx) => ctx.restore(),
		    getContext: (elem, type) => elem.getContext(type),
		    setFillColor: (ctx, color) => ctx.fillStyle = color,
		    setBorderColor: (ctx, color) => ctx.strokeStyle = color,
		    setFont: (ctx, font) => ctx.font = font,
		    shadowBlur: (ctx, blur_num) => ctx.shadowBlur = blur_num,
		    shadowColor: (ctx, color) => ctx.shadowColor = color,
		    setAlpha: (ctx, alpha) => ctx.globalAlpha = alpha,
		    setGlobalCompositeOperation: (ctx, operation) => ctx.globalCompositeOperation = operation,
		    setTextAlign: (ctx, align) => ctx.textAlign = align,
		    clearRect: (ctx, x, y, w, h) => ctx.clearRect(x, y, w, h),
		    fillRect: (ctx, x, y, w, h) => ctx.fillRect(x, y, w, h),
		    fillText: (ctx, text, x, y) => ctx.fillText(text, x, y),
		    strokeText: (ctx, text, x, y) => ctx.strokeText(text, x, y),
		    drawImage: (ctx, image, sx, sy, sw, sh, dx, dy, dw, dh) => ctx.drawImage(image, sx, sy, sw, sh, dx, dy, dw, dh),
		    drawImage_simple: (ctx, image, dx, dy) => ctx.drawImage(image, dx, dy),
		    setScale: (ctx, sx, sy) => ctx.scale(sx, sy),
		    setTransform: (ctx, a, b, c, d, e, f) => ctx.setTransform(a, b, c, d, e, f),
		    measureText: (ctx, line) => ctx.measureText(line),
		    setImageSmoothingEnabled: (ctx, enabled) => ctx.imageSmoothingEnabled = (enabled == 1),
		    shadowOffsetX: (ctx, offset) => ctx.shadowOffsetX = offset,
		    shadowOffsetY: (ctx, offset) => ctx.shadowOffsetY = offset,
		},
		math: {
		    random: () => Math.random()
		},
		treeWalker: {
		    currentNode(walker) { return walker.currentNode; },
		    setCurrentNode(walker, node) { walker.currentNode = node; },
		    nextNode(walker) { return walker.nextNode(); },
		    firstChild(walker) { return walker.firstChild(); },
		    nextSibling(walker) { return walker.nextSibling(); }
		}
	    }
	})
	proc.call_async();
    } catch(e) {
	if(e instanceof WebAssembly.CompileError) {
	    document.getElementById("wasm-error").hidden = false;
	}
	console.log(1);
	throw e;
    }
});
