const rawInput = 
//'     1,15,A,5,B,10,A,8,A,17,B,17 '
//'     100,0,C,100,C,115,C,119,C,121,C,144,C,154,C,157,G,158,C,171,C,179,C,194,C,206,C,214,C,227,C,229,C,231,C,298 '
//'     100,325,C,100,C,115,C,119,C,121,C,144,C,154,C,157,G,158,C,171,C,179,C,194,C,206,C,214,C,227,C,229,C,231,C,298 '
//'     100,160,C,100,C,115,C,119,C,121,C,144,C,154,C,157,G,158,C,171,C,179,C,194,C,206,C,214,C,227,C,229,C,231,C,298 '
'     1,0,nepper,15,hamster,24,philipp,30,mmautne,31,hamster,49,hamster,55,thebenil,57,fliegimandi,59,ev,61,philipp,64,philipp,65,ev,74,philipp,69,philipp,71,fliegimandi,78,hamster,78,mio,95,hamster,103,macquereauxpl,135 '
//'     1,120,6a,17,kl,5,kl,10,kl,15,cs,28,kl,20,kl,25,hr,35,hr,40,hr,41,hl,42,hr,43,hr,44,hl,44,hl,49,hr,47 '
//'     1,47,6a,17,kl,5,kl,10,kl,15,cs,28,kl,20,kl,25,hr,35,hr,40,hr,41,hl,42,hr,43,hr,44,hl,44,hl,49,hr,47 '

const input = rawInput.split(',');

const startBid = parseInt(input[0]);
const instantSell = parseInt(input[1]);
const bids = input.slice(2).reduce((acc, cur, i) => {
    if (i % 2) {
        acc[Math.floor(i / 2)].value = parseInt(cur);
    } else {
        acc.push({ bidder: cur });
    }

    return acc;
}, []);

console.log({ bids, startBid })

let currentPrice = startBid;
let highestBid = null;

let history = [ '-,' + startBid ];

console.log(`${'bidder'.padEnd(16, ' ')} ${'bid'.padEnd(16, ' ')} ${'current'.padEnd(16, ' ')} ${'highest bidder'.padEnd(16, ' ')} ${'history entry'.padEnd(16, ' ')}`);

for (const bid of bids) {
    if (highestBid === null) {
        highestBid = bid;

        history.push(bid.bidder + ',' + currentPrice);

        console.log(`${bid.bidder.toString().padEnd(16, ' ')} ${bid.value.toString().padEnd(16, ' ')} ${currentPrice.toString().padEnd(16, ' ')} ${highestBid.bidder.toString().padEnd(16, ' ')} true`);

        continue;
    }

    let historyEntry = true;

    // current bidder is also the highest bidder
    const isHighestBidder = bid.bidder === highestBid.bidder;

    if (bid.value > highestBid.value) {
        if (isHighestBidder) {
            // the current highest bidder is increasing their bid, we don't 
            // want a history entry in this case
            historyEntry = false;
        } else {
            // highest bidder changed, adjust current price
            currentPrice = highestBid.value + 1;
        }

        // this bid is the new highest bid
        highestBid = bid;
    } else if (bid.value === highestBid.value && !isHighestBidder) {
        // this bid is just as high as the current highest one,
        // but doesn't come from the current highest bidder
        // set the price to the maximum bid but don't change bidder

        currentPrice = bid.value;
    } else if (currentPrice <= bid.value) {
        // this bid is smaller than the highest bid but the current price is not 
        // larger than it, increase by one

        currentPrice = bid.value + 1;
    }

    let shouldBreak = false;

    if (currentPrice >= instantSell && instantSell !== 0) {
        currentPrice = instantSell;
        shouldBreak = true;
    }

    if (historyEntry) {
        history.push(highestBid.bidder + ',' + currentPrice);
    }

    if (shouldBreak) {
        break;
    }

    console.log(`${bid.bidder.toString().padEnd(16, ' ')} ${bid.value.toString().padEnd(16, ' ')} ${currentPrice.toString().padEnd(16, ' ')} ${highestBid.bidder.toString().padEnd(16, ' ')} ${historyEntry}`);

}

console.log({ highestBid, currentPrice });
console.log(history.join(','));
