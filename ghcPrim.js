
// utils

function toHsBool (b) {
    if (b) {
        return glConsValue("True");
    } else {
        return glConsValue("False");
    };
};

function primFunction1(f) {
    return glWhnfTerm(function (a) {
        var t = {};
        t.evalStep = function () {
            if (typeof(a.value) == "undefined") {
                a.evalStep();
                return;
            };
            this.value = f(a.value);
        };
        t.toStr = function () {
            return "primop1";
        };
        return t;
    })
};

function primFunction2(f) {
    return glWhnfTerm(function (a) {
        return glWhnfTerm(function (b) {
            var t = {};
            t.evalStep = function () {
                if (typeof(a.value) == "undefined") {
                    a.evalStep();
                    return;
                };
                if (typeof(b.value) == "undefined") {
                    b.evalStep();
                    return;
                };
                this.value = f(a.value, b.value);
            };
            t.toStr = function () {
                return "primop2";
            };
            return t;
        })
    })
};

function primFunction3(f) {
    return glWhnfTerm(function (a) {
        return glWhnfTerm(function (b) {
            return glWhnfTerm(function (c) {
                var t = {};
                t.evalStep = function () {
                    if (typeof(a.value) == "undefined") {
                        a.evalStep();
                        return;
                    };
                    if (typeof(b.value) == "undefined") {
                        b.evalStep();
                        return;
                    };
                    if (typeof(c.value) == "undefined") {
                        c.evalStep();
                        return;
                    };
                    this.value = f(a.value, b.value, c.value);
                };
                t.toStr = function () {
                    return "primop3";
                };
                return t;
            })
        })
    })
};

var f = null; // tmp variable


// wired-in error function

f = function (hsString) {
    throw "error of hsString";
};
ghczmprim.g_base_GHC_Err_error = primFunction1(f);


// * javascript implementation of GHC.Prim

// ** Addr#

// indexCharOffAddr#
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
ghczmprim.g_ghczmprim_GHC_Prim_indexCharOffAddrzh = primFunction2(f);

// ** Int#

// +#
f = function (a, b) {
    return (a + b);
};
ghczmprim.g_ghczmprim_GHC_Prim_zpzh = primFunction2(f);

// -#
f = function (a, b) {
    return (a - b);
};
ghczmprim.g_ghczmprim_GHC_Prim_zmzh = primFunction2(f);

// *#
f = function (a, b) {
    return (a * b);
};
ghczmprim.g_ghczmprim_GHC_Prim_ztzh = primFunction2(f);

// negateInt#
f = function (i) {
    return (-i);
};
ghczmprim.g_ghczmprim_GHC_Prim_negateIntzh = primFunction1(f);

// remInt#
f = function (a, b) {
    return (a % b);
};
ghczmprim.g_ghczmprim_GHC_Prim_remIntzh = primFunction2(f);

// >=#
f = function (a, b) {
    return toHsBool(a >= b);
};
ghczmprim.g_ghczmprim_GHC_Prim_zgzezh = primFunction2(f);

// >#
f = function (a, b) {
    return toHsBool(a > b);
};
ghczmprim.g_ghczmprim_GHC_Prim_zgzh = primFunction2(f);

// ==#
f = function (a, b) {
    return toHsBool(a == b);
};
ghczmprim.g_ghczmprim_GHC_Prim_zezezh = primFunction2(f);

// ** Word#

// minusWord#
f = function (a, b) {
    return (a - b);
};
ghczmprim.g_ghczmprim_GHC_Prim_minusWordzh = primFunction2(f);

// ltWord#
f = function (a, b) {
    return toHsBool(a < b);
};
ghczmprim.g_ghczmprim_GHC_Prim_ltWordzh = primFunction2(f);

// gtWord#
f = function (a, b) {
    return toHsBool(a > b);
};
ghczmprim.g_ghczmprim_GHC_Prim_gtWordzh = primFunction2(f);

// eqWord#
f = function (a, b) {
    return toHsBool(a == b);
};
ghczmprim.g_ghczmprim_GHC_Prim_eqWordzh = primFunction2(f);

// word2Int#
f = function (w) {
    return w;
};
ghczmprim.g_ghczmprim_GHC_Prim_word2Intzh = primFunction1(f);

/* This is very buggy. It does not do the same thing as GHC.Prim
// uncheckedShiftL#
f = function (w, i) {
    console.log(w + " << " + i, w << i);
    return (w << i);
};
ghczmprim.g_ghczmprim_GHC_Prim_uncheckedShiftLzh = primFunction2(f);

// uncheckedShiftRL#
f = function (w, i) {
    console.log(w + " >> " + i, w >> i);
    return (w >> i);
};
ghczmprim.g_ghczmprim_GHC_Prim_uncheckedShiftRLzh = primFunction2(f);

// or#
f = function (a, b) {
    return (a | b);
};
ghczmprim.g_ghczmprim_GHC_Prim_orzh = primFunction2(f);

*/
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
        throw "error: " + stringToAddr(msg);
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

jPrim = {
    consoleLog: function (msg, realWorld) {
        console.log(msg);
        return [null, realWorld];
    },
};
