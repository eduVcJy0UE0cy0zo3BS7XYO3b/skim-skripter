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

var _load;
if (typeof load !== 'undefined') {
  _load = load;
} else if (typeof require !== 'undefined') {
  _load = require;
}

// V8 treats multiple arguments as files, unless -- is given, but
// SpiderMonkey doesn't treat -- specially.  This is a hack to allow
// for -- on SpiderMonkey.
if (args[0] == '--') {
    args.shift();
}

if (args.length < 3) {
    logErr('usage: call.js REFLECT_JS_DIR REFLECT_WASM_DIR PROC.WASM ARG.WASM...');
    _exit(1);
}

async function runTest(call, opts) {
    try {
        let [procFile, ...argFiles] = call;
        let [proc] = await Scheme.load_main(procFile, opts);
        let argPromises =
            argFiles.map(file => proc.reflector.load_extension(file, opts));
        let args = [];
        for (let p of argPromises) {
            let [arg] = await p;
            args.push(arg);
        }
        for (let result of proc.call(...args))
            log(repr(result));
    } catch (e) {
        if (e instanceof hoot.SchemeQuitError) {
            _exit(e.status);
        } else {
            log(`error: ${e} (${e.stack})`);
            _exit(1);
        }
    }
}

var [reflect_js_dir, reflect_wasm_dir, ...test_call] = args;
var hoot = _load(`${reflect_js_dir}/reflect.js`);
if (typeof hoot !== 'undefined') {
  Scheme = hoot.Scheme;
  repr = hoot.repr;
}
waitFor(runTest(test_call, {reflect_wasm_dir}));
