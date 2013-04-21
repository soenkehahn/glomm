
function glommConstructorFunction (name) {
    var t = {};
    t.isConstructed = true;
    t.glommConstructorName = name;
    t.glommConstructorArgs = [];
    
    t.toString = function () {
        return ("Cons: " + name);
    };
    return t;
};

function glommShowConstructor(c) {
    if (! c.isWhnf) {
        c = c.forceWhnf();
    };
    if (typeof c.value.glommConstructorName == "undefined") {
        throw "name undefined: " + c.toString();
    };
    var r = c.value.glommConstructorName;
    for (var i in c.value.glommConstructorArgs) {
        r = r + " @ " + glommShowConstructor(c.value.glommConstructorArgs[i]);
    };
    return r;
};

function glommFromWhnf(o) {
    var t = {};
    t.isWhnf = true;
    t.value = o;
    t.forceWhnf = function () {
        return t;
    };
    t.toString = function () {
        return ("whnf: " + t.value);
    };
    return t;
};

function glommQuoted(quoted) {
    var t = {};
    t.isWhnf = false;
    t.forceWhnf = function () {
        var result = quoted();
        if (result.isWhnf) {
            return result;
        } else {
            return result.forceWhnf();
        };
    };
    t.toString = function () {
        return "thunk";
    };
    return t;
};

function glommApply(f, x) {
    var t = {};
    t.isWhnf = false;
    t.forceWhnf = function () {
        if (! f.isWhnf) {
            f = f.forceWhnf();
        }
        if (f.value.isConstructed) {
            // constructor application
            f.value.glommConstructorArgs.push(x);
            f.isWhnf = true;
            return f;
        } else {
            // beta reduction
            return ((f.value)(x)).forceWhnf();
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
