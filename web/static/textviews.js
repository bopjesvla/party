export function renderVote(players, vote) {
  let targets = vote.opt.map(t => {
		if (typeof t == "number") {
			return slotName(players, t)
		}
		else {
			return t
		}
	})
	return `${vote.act} ${vote.opt.join(", ")}`
}

export function slotName(players, slot) {
  return this.info.players.filter(x => x.slot == slot)[0].name
}
