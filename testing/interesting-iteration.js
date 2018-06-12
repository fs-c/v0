const exec = async (i) => {
    console.log(i);

    return ++i;
};

(async () => {

let i = 0;
while ((i = await exec(i)) < 10);

})();