module.exports = [
  { // Harvests energy and transfers it to the spawn or spawn extensions.
    name: 'harvester',
    parts: [ WORK, CARRY, MOVE ],
    min: 1
  },
  { // Harvests energy and uses it to upgrade the room controller.
    name: 'upgrader',
    parts: [ WORK, CARRY, MOVE ],
    min: 1
  }
]
