// -*- js-indent-level: 4 -*-

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

if ((args.length < 3) || (args.length > 4)) {
    logErr('usage: load.js REFLECT_JS_DIR REFLECT_WASM_DIR FOO.WASM [USER_IMPORTS]');
    _exit(1);
}

async function runTest(wasmFile, opts) {
    try {
        for (let obj of await Scheme.load_main(wasmFile, opts))
            log(repr(obj));
    } catch (e) {
        if (e instanceof hoot.SchemeQuitError) {
            _exit(e.status);
        } else {
            log(`error: ${e} (${e.stack})`);
            _exit(1);
        }
    }
}

var [reflect_js_dir, reflect_wasm_dir, test_wasm, user_imports_file] = args;
var hoot = _load(`${reflect_js_dir}/reflect.js`);
if (typeof hoot !== 'undefined') {
    Scheme = hoot.Scheme;
    repr = hoot.repr;
}
var user_imports = {};
if (user_imports_file) {
    user_imports = _load(user_imports_file).user_imports;
    if (typeof user_imports === 'undefined') {
        logErr(`user imports file ${user_imports_file} failed to load`);
        _exit(1);
    }
}
waitFor(runTest(test_wasm, {reflect_wasm_dir, user_imports}));
