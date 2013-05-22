
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
    if (c.value.glConsName == "JIO") {
        return glExecuteJIO(c);
    };
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
        r += " @ (" + glExecuteMain(c.value.glConsArgs[i]) + ")"; 
    };
    return r;
};

function glExecuteJIO(jio) {
    realWorld = {value: 42};
    toWhnf(jio);
    stateFunction = jio.value.glConsArgs[0];
    toWhnf(stateFunction);
    result = glApplyTerm(stateFunction, realWorld);
    toWhnf(result);
    return result.value[0];
};

function toWhnf(t) {
    while (typeof(t.value) == "undefined") {
        t.toWhnff();
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
        toWhnff: function () {
            var result = quoted();
            this.value = result.value;
            this.toWhnff = result.toWhnff;
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
    t.toWhnff = function () {
        if (typeof(f.value) == "undefined") {
            f.toWhnff();
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
                this.toWhnff = result.toWhnff;
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
    t.toWhnff = function () {
        if (typeof(scrutinee.value) == "undefined") {
            scrutinee.toWhnff();
            return;
        } else {
            var result = rhsFun(scrutinee);
            this.value = result.value;
            this.toWhnff = result.toWhnff;
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
