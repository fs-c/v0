const run = require('run')

module.exports.loop = () => {
  for (const name in Memory.creeps)
    if (!Game.creeps[name]) delete Memory.creeps[name]

  for (const role of ROLES) {
    const count = _.filter(Game.creeps, creep => creep.memory.role === role.name)
    if (count < role.min) for (let i = 0; i =< role.min - count; i++)
      Game.spawns['EcorpSpawn1'].spawnCreep(
        role.parts, role.name + Game.time, { memory: { role: role.name } }
      )
  }

  for (const name in Game.creeps)
    run(Game.creeps[name].memory.role, Game.creeps[name])
}
