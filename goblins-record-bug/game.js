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
	    }
	})
    } catch(e) {
	if(e instanceof WebAssembly.CompileError) {
	    document.getElementById("wasm-error").hidden = false;
	}
	console.log(1);
	throw e;
    }
});
