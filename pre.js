
function glommConstructor (name) {
    var f = function (arg) {
        f.glommConstructorArgs.push(arg);
        return f;
    };
    f.glommConstructorName = name;
    f.glommConstructorArgs = [];
    return f;
};


function glommShowConstructor(c) {
    var r = c.glommConstructorName;
    for (var i in c.glommConstructorArgs) {
        r = r + " @ " + glommShowConstructor(c.glommConstructorArgs[i])
    }
    return r
};

function patError (msg) {
    throw ("Non-exhaustive patterns in " + glommFullyForce(msg));
}

function glommFromWhnf(o) {
    var t = {};
    t.isForced = true;
    t.value = o;
    return t;
};

function glommQuoted(quoted) {
    var t = {};
    t.isForced = false;
    t.forceSome = function () {
        return quoted();
    };
    return t;
};

function glommApply(f, x) {
    var t = {};
    t.isForced = false;
    t.forceSome = function () {
        if (! f.isForced) {
            return glommApply(f.forceSome(), x);
        } else {
            return (f.value)(x);
        };
    };
    return t;
};

function glommFullyForce(o) {
    if (o.isForced) {
        return o.value;
    } else {
        return glommFullyForce(o.forceSome());
    };
};
