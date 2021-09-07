import { getOverviewFiles, getCoordinator } from '../lib/index.js';

(async () => {

const overviewFiles = await getOverviewFiles('N:\\1. NACHMITTAGSBETREUUNG');

console.log({ overviewFiles, overviewFilesLength: overviewFiles.length });

for (const filePath of overviewFiles) {
    const coordinator = await getCoordinator(filePath);

    console.log(coordinator);
}

})().then().catch(console.error)
