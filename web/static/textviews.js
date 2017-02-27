export function renderVote(vote, players) {
  let targets = vote.opt.map(t => {
		if (typeof t == "number") {
			return slotName(t, players)
		}
		else if (t == "noone") {
			return "no one"
		}
		else {
			return t
		}
	})
	return `${vote.act} ${targets.join(", ")}`
}

export function slotName(slot, players) {
  console.log(arguments);
  return players.filter(x => x.slot == slot)[0].name
}
