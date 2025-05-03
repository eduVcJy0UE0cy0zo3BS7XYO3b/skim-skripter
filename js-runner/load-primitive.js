var waitFor;
if (typeof drainJobQueue !== 'undefined') {
  waitFor = function waitFor(p) { drainJobQueue(); return p; };
} else {
  // JSC and V8 will drain promises before exiting and don't require a
  // specific waiter.
  waitFor = function waitFor(p) { return p; };
}

var args;
if (typeof process !== 'undefined') {
    args = process.argv.slice(3);
} else if (typeof scriptArgs !== 'undefined') {
    args = scriptArgs;
} else if (typeof arguments !== 'undefined') {
    args = arguments;
} else {
    // No script arguments available
    args = [];
}

var log;
var logErr;
if (typeof print !== 'undefined') {
    log = print;
} else if (typeof console !== 'undefined') {
    log = console.log.bind(console);
}
if (typeof printErr !== 'undefined') {
    logErr = printErr;
} else {
    logErr = log;
}

var _exit;
if (typeof quit !== 'undefined') {
    _exit = quit.bind(this);
} else if (typeof testRunner !== 'undefined') {
    _exit = testRunner.quit.bind(testRunner);
} else if (typeof process !== 'undefined') {
    _exit = process.exit.bind(process);
}

// V8 treats multiple arguments as files, unless -- is given, but
// SpiderMonkey doesn't treat -- specially.  This is a hack to allow
// for -- on SpiderMonkey.
if (args[0] == '--') {
  args.shift();
}

if (args.length < 2) {
  logErr('usage: load-primitive.js FOO.WASM FUNC [ARGS ...]');
  _exit(1);
}

async function instantiateStreaming(path, imports) {
  if (typeof fetch !== 'undefined' && typeof window !== 'undefined')
    return WebAssembly.instantiateStreaming(fetch(path), imports);
  let bytes;
  if (typeof read !== 'undefined') {
    bytes = read(path, 'binary');
  } else if (typeof readFile !== 'undefined') {
    bytes = readFile(path);
  } else {
    let fs = require('fs');
    bytes = fs.readFileSync(path);
  }

  return WebAssembly.instantiate(bytes, imports);
}

async function compileAndRun(wasmFile, funcName, args) {
  const imports = {};
  const { module, instance } = await instantiateStreaming(wasmFile, imports);
  const f = instance.exports[funcName];
  return f.apply(null, args);
}

async function runTest(wasmFile, funcName, ...args) {
  const parsedArgs = args.map(JSON.parse);
  try {
    const result = await compileAndRun(wasmFile, funcName, parsedArgs);
    log(result.toString());
  } catch (e) {
      log(`error: ${e} (${e.stack})`);
      _exit(1);
  }
}

waitFor(runTest.apply(null, args));
