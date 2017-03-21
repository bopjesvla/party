export function renderVote(vote, players) {
  console.log(arguments);
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

export function renderEnd(winners, players) {
  let targets = winners.map(t => {
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
	return `${targets.join(" & ")} won`
}

export function slotName(slot, players) {
  let player = players.filter(x => x.slot == slot && x.status == "playing")[0] ||
    players.filter(x => x.slot == slot && x.status != "out")[0]
  return player.name
}
