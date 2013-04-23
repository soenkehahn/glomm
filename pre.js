
function glommConstructorFunction (name) {
    var t = {};
    t.isConstructed = true;
    t.glommConstructorName = name;
    t.glommConstructorArgs = [];

    t.toString = function () {
        var r = "Cons: " + name;
        for (i in t.glommConstructorArgs) {
            r = r + " @ " + t.glommConstructorArgs[i].toString();
        };
        return r;
    };
    return t;
};

// this is a bit magical atm. When we have a good idea how the entry point is typed, this can be implemented better.
function glommShowConstructor(c) {
    c.toWhnf();
    if (typeof c.value == "number") {
        return ("Prim: " + c.value);
    };
    if (typeof c.value == "string") {
        return ("Prim: " + c.value);
    };
    if (typeof c.value.glommConstructorName == "undefined") {
        throw "name undefined: " + c.value + " " + typeof(c.value);
    };
    var r = c.value.glommConstructorName;
    for (var i in c.value.glommConstructorArgs) {
        r += " @ " + glommShowConstructor(c.value.glommConstructorArgs[i]);
    };
    return r;
};

function glommFromWhnf(o) {
    var t = {};
    t.value = o;

    t.toWhnf = function () {
    };
    t.toString = function () {
        return ("whnf: " + t.value);
    };
    return t;
};

function glommQuoted(quoted) {
    var t = {};
    t.toWhnf = function () {
        if (typeof(t.value) != "undefined") {
            return;
        };
        var result = quoted();
        result.toWhnf();
        if (typeof(result.value.toWhnf) != "undefined") {
            throw "nested";
        };
        t.value = result.value;
    };
    t.toString = function () {
        return "thunk";
    };
    return t;
};

function glommApply(f, x) {
    var t = {};
    t.toWhnf = function () {
        if (typeof(t.value) != "undefined") {
            return;
        };
        f.toWhnf();
        if (typeof(f.value.toWhnf) != "undefined") {
            throw "nested";
        };
        if (f.value.isConstructed) {
            f.value.glommConstructorArgs.push(x);
            t.value = f.value;
        } else {
            // beta reduction
            var result = (f.value)(x)
            result.toWhnf();
            t.value = result.value;
        };
    };
    t.toString = function () {
        return ("(" + f.toString() + " @ " + x.toString() + ")");
    };
    return t;
};

function patError (msg) {
    throw ("Non-exhaustive patterns in " + msg);
}
