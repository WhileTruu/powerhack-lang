const test = require('ava');
const {exec} = require('../setup');

test(
    'unification fail',
    exec,
    __dirname,
    ['Main.powerhack', '--output', 'out.js'],
    async (program, t) => {
        const snapshot = await t.throwsAsync(program);
        t.is(snapshot.code, 1);
        await t.context.cliSnapshot(snapshot);
    }
);
