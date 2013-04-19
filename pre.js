

function konstruktor (name) {
    var f = function (arg1) {
        var f = function (arg2) {
            console.log("innermost")
        }
        f.namee = name
        f.argss = [arg1]
        return f
    }
    f.namee = name
    f.argss = []
    return f
}

function showKons(k) {
    var r = k.namee
    for (var i in k.argss) {
        r = r + " @ " + showKons(k.argss[i])
    }
    return r
}

function patError() {
    return function(msg) {
        throw msg
    }
}

function unpackCStringzh(x) {return x}

/*
 * testing
var t = konstruktor("huhu")(konstruktor("bubu"))
console.log(t)
console.log(showKons(t))
*/
