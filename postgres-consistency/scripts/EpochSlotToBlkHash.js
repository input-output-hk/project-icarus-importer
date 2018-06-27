var fetch = require("node-fetch");

const serverRoute = 'https://explorer.iohkdev.io/api';

const slotsPerEpoch = 21600;

const randomNumberWithMax = function (topNumber) {
  return Math.floor((Math.random() * topNumber));
}

function removeNullFromArray(actual) {
  var newArray = [];
  for (var i = 0; i < actual.length; i++) {
    if (actual[i]) {
      newArray.push(actual[i]);
    }
  }
  return newArray;
}

const generateRandomEpochAndSlots = function (topEpoch, numGenerated) {
  const epochAndSlotGenerated = [];
  for (let i = 0; i < numGenerated; i++) {
    const epochGen = randomNumberWithMax(Number(topEpoch) + 1);
    const slotGen = randomNumberWithMax(slotsPerEpoch);
    epochAndSlotGenerated.push([epochGen, slotGen]);
  }
  return epochAndSlotGenerated;
};

const parseResponse = function (response) {
  return response.json();
};

const handleErrors = function (responseJson) {
  if (responseJson.error) {
    console.error(`[ExplorerApi.handleErrors] error[${responseJson.error.name}]`);
    throw responseJson.error;
  } else {
    return Promise.resolve(responseJson);
  }
};

const epochAndSlotToBlkHash = async function (epochNumber, slotNumber) {
  return fetch(`${serverRoute}/epochs/${epochNumber}/${slotNumber}`, {
    method: 'GET',
    headers: {
      'Content-Type': 'application/json;charset=utf-8',
    },
  })
  .then(parseResponse)
  .then(handleErrors)
  .then(response => {
    try {
      const { Right: [{ cbeBlkHash }] } = response;
      return cbeBlkHash;
    } catch (e) {
      console.error(`Epoch ${epochNumber} and slot ${slotNumber} have no block`);
      return null;
    }
  });
};

//NOTE: includes topEpoch in the possible epochs to be generated
const getRandomBlkHashes = function (topEpoch, numGenerated) {
  const epochAndSlotGen = generateRandomEpochAndSlots(topEpoch, numGenerated);
  
  return Promise.all(epochAndSlotGen.map(([epochGen, slotGen]) =>
            epochAndSlotToBlkHash(epochGen, slotGen))).then(removeNullFromArray);
};

// FIXME: Sometimes fails, why?
// FIXME: Log error separately
// FIXME: Describe usage
//        Example: node ./postgres-consistency/scripts/EpochSlotToBlkHash.js 10 10 
getRandomBlkHashes(process.argv[2], process.argv[3]).then((blkHashes) =>
  blkHashes.map(hash => console.log(hash)));