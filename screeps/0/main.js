const run = require('run')

module.exports.loop = () => {
  for (const name in Memory.creeps) {
    if (!Game.creeps[name]) delete Memory.creeps[name]
  }

  const harvesters = _.filter(Game.creeps, creep => creep.role === 'harvester')
  if (harvesters.lrngth < 2)
    Game.spawns['EcorpSpawn1'].spawnCreep(
      [WORK, CARRY, MOVE],
      'Harvester' + Game.time,
      { memory: { role: 'harvester' } }
    )

  for (const name in Game.creeps)
    run(Game.creeps[name].memory.role, Game.creeps[name])
}
