
// utils

function toHsBool (b) {
    var r;
    if (b) {
        r = glommConstructorFunction("True");
    } else {
        r = glommConstructorFunction("False");
    };
    return r;
};

function primFunction1(f) {
    return glommFromWhnf(function (a) {
        a.toWhnf();
        return glommFromWhnf(f(a.value));
    })
};

function primFunction2(f) {
    return glommFromWhnf(function (a) {
        return glommFromWhnf(function (b) {
            var t = {};
            t.toWhnff = function () {
                if (typeof(a.value) == "undefined") {
                    a.toWhnff();
                    return;
                };
                if (typeof(b.value) == "undefined") {
                    b.toWhnff();
                    return;
                };
                this.value = f(a.value, b.value);
            };
            return t;
        })
    })
};

var f = null; // tmp variable


// wired-in error function

f = function (hsString) {
    throw "error of hsString";
};
var g_base_GHC_Err_error = primFunction1(f);

// javascript implementation of GHC.Prim

f = function (addr, offset) {
    if (offset > addr.length + 5) {
        throw "segfault";
    };
    if (offset >= addr.length) {
        return 0;
    } else {
        return addr.charCodeAt(offset);
    };
};
var g_ghczmprim_GHC_Prim_indexCharOffAddrzh = primFunction2(f);

f = function (a, b) {
    return (a + b);
};
// +#
var g_ghczmprim_GHC_Prim_zpzh = primFunction2(f);

f = function (a, b) {
    return (a - b);
};
// -#
var g_ghczmprim_GHC_Prim_zmzh = primFunction2(f);

f = function (a, b) {
    return toHsBool(a >= b);
};
// >=#
var g_ghczmprim_GHC_Prim_zgzezh = primFunction2(f);

f = function (a, b) {
    return toHsBool(a > b);
};
// >#
var g_ghczmprim_GHC_Prim_zgzh = primFunction2(f);

f = function (a, b) {
    return toHsBool(a == b);
};
// ==#
var g_ghczmprim_GHC_Prim_zezezh = primFunction2(f);

f = function (a, b) {
    return toHsBool(a < b);
};
// ltWord#
var g_ghczmprim_GHC_Prim_ltWordzh = primFunction2(f);

f = function (a, b) {
    return toHsBool(a > b);
};
// gtWord#
var g_ghczmprim_GHC_Prim_gtWordzh = primFunction2(f);

f = function (a, b) {
    return toHsBool(a == b);
};
// eqWord#
var g_ghczmprim_GHC_Prim_gtWordzh = primFunction2(f);
