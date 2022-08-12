# Snapshot report for `integration-tests/function/test.test.js`

The actual snapshot is saved in `test.test.js.snap`.

Generated by [AVA](https://avajs.dev).

## function

> Snapshot 1

    'powerhack Main.powerhack --output out.js'

> Snapshot 2

    ''

> Snapshot 3

    `Success! Compiled 1 module.␊
    ␊
        Main.powerhack ───> out.js␊
    ␊
    `

> Snapshot 4

    `#!/usr/bin/env node␊
    ␊
    var add = function (b) { return function (a) { return a + b; }; }␊
    var sub = function (b) { return function (a) { return a - b; }; }␊
    var eq = function (b) { return function (a) { return a === b; }; }␊
    ␊
    var foo = function (a) {␊
        return function (b) {␊
            return function (c) {␊
                return function (d) {␊
                    return 3␊
                }␊
            }␊
        }␊
    }␊
    ␊
    console.log((function () { return main() })())`