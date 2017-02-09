<template>
	<form @submit.prevent=start>
		<button type="submit">New Game</button>
	</form>
</template>
<script>
	import socket from '../socket'

  let test_setup = {
    setup: {
      roles: [],
      phases: ["d", "n"],
      teams: [{player: 1, team: "m"}, {player: 2, team: "t"}, {player: 3, team: "t"}, {player: 4, team: "t"}]
    }
  }

	let generateID = function() {
		let d = new Date().getTime();
		let uuid = ""
		for (let i = 0; i < 8; i++) {
			let r = (d + Math.random()*36)%36 | 0;
			d = Math.floor(d/36);
			uuid += r.toString(36)
		}
		return uuid;
	};

	export default {
		methods: {
			start() {
				let id = generateID()
				socket.channel(`game:${id}`, test_setup).join()
					.receive("ok", _ => this.$router.push({name: 'game', params: {name: id}}))
					.receive("error", x => console.log(x))
			}
		}
	}
</script>
