module.exports = (role, creep) => {
  try {
    require(role + '.role').run(creep)
  } catch (e) { console.error(`attempted to run bad role (${role}).`) }
}
