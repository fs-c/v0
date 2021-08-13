const vm = require('vm');
const fs = require('fs');

require('./test-import')

const mod = new vm.SourceTextModule(`
    import * as _ from './test-import';
`);

(async () => {

console.log('linking')

await mod.link(async (specifier, ref) => {
    try {
        console.log({specifier,ref})

        const path = require.resolve(specifier);

        console.log(path);

        return new vm.SourceTextModule(await fs.readFile(path), {
            context: ref.context,
        });
    } catch (err) {
        console.error(err);
    }
});

console.log('evaluating')

await mod.evaluate();

})().then().catch(console.error)
