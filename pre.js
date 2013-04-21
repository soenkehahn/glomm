
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
    if (typeof c.value.glommConstructorName == "undefined") {
        throw "name undefined: " + c.toString();
    };
    var r = c.value.glommConstructorName;
    for (var i in c.value.glommConstructorArgs) {
        r = r + " @ " + glommShowConstructor(glommForceWhnf(c.value.glommConstructorArgs[i]));
    };
    return r;
};

function glommFromWhnf(o) {
    var t = {};
    t.isWhnf = true;
    t.value = o;
    t.toString = function () {
        return ("whnf: " + t.value);
    };
    return t;
};

function glommQuoted(quoted) {
    var t = {};
    t.isWhnf = false;
    t.forceSome = function () {
        return quoted();
    };
    t.toString = function () {
        return "thunk";
    };
    return t;
};

function glommApply(f, x) {
    var t = {};
    t.isWhnf = false;
    t.forceSome = function () {
        if (! f.isWhnf) {
            return glommApply(f.forceSome(), x);
        } else {
            if (f.value.isConstructed) {
                // constructor application
                f.value.glommConstructorArgs.push(x);
                f.isForced = true;
                return f;
            } else {
                // beta reduction
                return (f.value)(x);
            };
        };
    };
    t.toString = function () {
        return ("(" + f.toString() + " @ " + x.toString() + ")");
    };
    return t;
};

function glommForceWhnf(o) {
    var x = o;
    while (! x.isWhnf) {
        var x = x.forceSome();
    };
    return x;
};

function patError (msg) {
    throw ("Non-exhaustive patterns in " + glommForceWhnf(msg));
}
