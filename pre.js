
// Returns a constructed value (not a term)
function glConsValue (name) {
    var t = {};
    t.isConstructed = true;
    t.glConsName = name;
    t.glConsArgs = [];

    t.toStr = function () {
        var r = "Cons: " + name;
        for (i in this.glConsArgs) {
            r = r + " @ (" + this.glConsArgs[i].toStr() + ")";
        };
        return r;
    };
    return t;
};

function cloneConsValue(consValue) {
    var r = glConsValue(consValue.glConsName);
    for (i in consValue.glConsArgs) {
        r.glConsArgs.push(consValue.glConsArgs[i]);
    };
    return r;
};


// this is a bit magical atm. When we have a good idea how the entry point is typed, this can be implemented better.
function glExecuteMain(c) {
    toWhnf(c);
    if (c.value.glConsName == "JIO" || c.value.glConsName == "JIOBind") {
        glExecuteJIO(c);
    } else {
        console.log(glShowMain(c));
    };
};

function glExecuteJIO(outerCommand) {
    realWorld = {value: hsUnit()};
    function exec(command, realWorld) {
        toWhnf(command);
        if (command.value.glConsName == "JIOBind") {
            a = command.value.glConsArgs[0];
            bf = command.value.glConsArgs[1];
            x = exec(a, realWorld);
            b = glApplyTerm(bf, x);
            return exec(b, realWorld);
        } else if (command.value.glConsName == "JIO") {
            stateFunction = command.value.glConsArgs[0];
            toWhnf(stateFunction);
            result = glApplyTerm(stateFunction, realWorld);
            toWhnf(result);
            return result.value.glConsArgs[0];
        };
    };
    exec(outerCommand, realWorld);
};

function glShowMain(c) {
    toWhnf(c);
    if (typeof c.value == "number") {
        return ("Prim: " + c.value);
    };
    if (typeof c.value == "string") {
        return ("Prim: " + c.value);
    };
    if (typeof c.value.glConsName == "undefined") {
        console.log ("name undefined");
        return c.toStr();
    };
    var r = c.value.glConsName;
    for (var i in c.value.glConsArgs) {
        r += " @ (" + glShowMain(c.value.glConsArgs[i]) + ")"; 
    };
    return r;
};

function toWhnf(t) {
    while (typeof(t.value) == "undefined") {
        t.evalStep();
    };
};

// Returns a term that is in whnf.
function glWhnfTerm(o) {
    var t = {};
    t.value = o;

    t.toStr = function () {
        if (typeof(t.value.isConstructed) == "undefined") {
            return ("whnf: " + t.value);
        } else {
            return ("whnf: " + t.value.toStr());
        };
    };
    return t;
};

// Returns a term that is a thunk.
function glQuotedTerm(quoted) {
    var t = {
        evalStep: function () {
            var result = quoted();
            this.value = result.value;
            this.evalStep = result.evalStep;
            this.toStr = result.toStr;
        },
        toStr: function () {
            return "thunk";
        }
    };
    return t;
};

// Returns a term that is an application of two terms.
function glApplyTerm(f, x) {
    var t = {};
    t.evalStep = function () {
        if (typeof(f.value) == "undefined") {
            f.evalStep();
            return;
        } else {
            if (f.value.isConstructed) {
                var clonedValue = cloneConsValue(f.value);
                clonedValue.glConsArgs.push(x);
                this.value = clonedValue;
                this.toStr = f.toStr;
            } else {
                // beta reduction
                var result = (f.value)(x)
                this.value = result.value;
                this.evalStep = result.evalStep;
                this.toStr = result.toStr;
            };
        };
    };
    t.toStr = function () {
        return ("(" + f.toStr() + " @ " + x.toStr() + ")");
    };
    return t;
};

// Returns a term for a case statement.
// The rhs expects the scrutinee in whnf.
function glCaseTerm (scrutinee, rhsFun) {
    var t = {};
    t.evalStep = function () {
        if (typeof(scrutinee.value) == "undefined") {
            scrutinee.evalStep();
            return;
        } else {
            var result = rhsFun(scrutinee);
            this.value = result.value;
            this.evalStep = result.evalStep;
            this.toStr = result.toStr;
        };
    };
    t.toStr = function () {
        return "case: " + scrutinee.toStr();
    };
    return t;
};

function patError (msg) {
    throw ("Non-exhaustive patterns in " + msg);
}

function assertNotNull (a, identifier) {
    if (typeof(a) == "undefined") {
        throw ("identifier not defined: " + identifier);
    };
    return a;
};


// JPrim

stringToAddr = function (value) {
    // value :: String in whnf
    var result = "";
    while (value.glConsName == "ZC") {
        element = value.glConsArgs[0];
        toWhnf(element);
        result += String.fromCharCode(element.value.glConsArgs[0].value);
        tail = value.glConsArgs[1];
        toWhnf(tail);
        value = tail.value;
    };
    return result;
};

jPrimTerms = {
    error: primFunction1(function (msg) {
        console.log("error: " + stringToAddr(msg));
        process.exit(1);
    }),
    patError: primFunction1(function (x) {
        throw "patError nyi";
    }),
    ffi1: primFunction2(function (s, arg) {
        fun = eval(stringToAddr(s));
        if (typeof(fun) == "undefined") {
            throw "does not exist: " + stringToAddr(s);
        };
        return fun(arg);
    }),
    ffi2: primFunction3(function (s, arg1, arg2) {
        fun = eval(stringToAddr(s));
        if (typeof(fun) == "undefined") {
            throw "does not exist: " + stringToAddr(s);
        };
        return fun(arg1, arg2);
    }),
};

function toJIO1(f){
    return function (arg1, realWorld) {
        r = f(arg1);
        return toHsTuple2(r, realWorld);
    };
};

jPrim = {
    jputStrLn: function (msg, realWorld) {
        console.log(msg);
        return toHsTuple2(hsUnit(), realWorld);
    },
    alert: toJIO1(function (msg) {
        if (typeof(alert) == "undefined") {
            console.log("alert: " + msg);
        } else {
            alert(msg);
        };
    }),
    prompt: toJIO1(function (msg) {
        return prompt(msg);
    }),
    jstringEquals: function(a, b) {
        return toHsBool(a == b);
    },
    jstringAppend: function(a, b) {
        return a + b;
    },
};
