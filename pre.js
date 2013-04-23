
// Returns a constructed value (not a term)
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
    toWhnf(c);
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

function toWhnf(t) {
    var c = 0;
    while (typeof(t.value) == "undefined") {
        c += 1;
        if (c > 10000) {
            // ~ throw "too many steps";
        };
        t.toWhnff();
    };
};

// Returns a term that is in whnf.
function glommFromWhnf(o) {
    var t = {};
    t.value = o;

    t.toString = function () {
        return ("whnf: " + t.value);
    };
    return t;
};

// Returns a term that is a thunk.
function glommQuoted(quoted) {
    var t = {
        toWhnff: function () {
            var result = quoted();
            this.value = result.value;
            this.toWhnff = result.toWhnff;
        },
        toString: function () {
            return "thunk";
        }
    };
    return t;
};

// Returns a term that is an application of two terms.
function glommApply(f, x) {
    var t = {};
    t.toWhnff = function () {
        if (typeof(f.value) == "undefined") {
            f.toWhnff();
            return;
        } else {
            if (f.value.isConstructed) {
                f.value.glommConstructorArgs.push(x);
                this.value = f.value;
            } else {
                // beta reduction
                var result = (f.value)(x)
                this.value = result.value;
                this.toWhnff = result.toWhnff;
            };
        };
    };
    t.toString = function () {
        return ("(" + f.toString() + " @ " + x.toString() + ")");
    };
    return t;
};

// Returns a term for a cast.
// The rhs expects the scrutinee in whnf.
function glommCast (scrutinee, rhsFun) {
    var t = {};
    t.toWhnff = function () {
        if (typeof(scrutinee.value) == "undefined") {
            scrutinee.toWhnff();
            return;
        } else {
            var result = rhsFun(scrutinee);
            this.value = result.value;
            this.toWhnff = result.toWhnff;
        };
    };
    return t;
};

function patError (msg) {
    throw ("Non-exhaustive patterns in " + msg);
}
