const test = require('ava');
const {exec} = require('../setup');

test(
    'definition in function',
    exec,
    __dirname,
    ['Main.powerhack', '--output', 'out.js'],
    async (program, t) => {
        const snapshot = await program;
        await t.context.cliSnapshot(snapshot);
    }
);