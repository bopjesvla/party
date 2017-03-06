<template>
	<table class="game-list">
	  <tr v-if="myGames.length">
	    <th colwidth="2">Your Games</th>
	  </tr>
		<tr v-for="game in myGames" @click="signup(game.id)">
			<td class="setup-name">{{game.id}}: {{game.setup}}</td>
			<td class="status">{{game.status}}</td>
		</tr>
		<tr v-if="gamesInSignups.length">
	    <th colwidth="2">Signups</th>
	  </tr>
		<tr v-for="game in gamesInSignups" @click="signup(game.id)">
			<td class="setup-name">{{game.id}}: {{game.setup}}</td>
			<td class="players">{{game.size - game.empty}}/{{game.size}}</td>
		</tr>
		<tr v-if="replacementRequests.length">
	    <th colwidth="2">Replacements</th>
	  </tr>
		<tr v-for="game in replacementRequests" @click="signup(game.id)">
			<td class="setup-name">{{game.id}}: {{game.setup}}</td>
		</tr>
	</table>
</template>

<script>
	import {queue_channel, user_channel} from "../socket"

	export default {
		data() {
			return {
				gamesInSignups: [],
				replacementRequests: [],
				myGames: []
			}
		},
		created() {
			user_channel.push("list:games", {})
				.receive("ok", d => this.myGames = d.games)
			queue_channel.on("games", d => {
			  this.gamesInSignups = d.signups
			  this.replacementRequests = d.replacements
			})
			// user_channel.push("list:games", {})
			// 	.receive("ok", d => this.gamesInSignups = d.games)
			queue_channel.on("status", msg => {
				let game = this.myGames.filter(x => x.id == msg.id)[0]
				if (game) {
				  game.status = msg.status
				}
			})
			user_channel.on("new:game", msg => {
				this.myGames.unshift(msg)
			})
			user_channel.on("leave:game", msg => {
				let game = this.myGames.filter(x => x.id == msg.id)[0]
				if (game) {
				  this.myGames.splice(this.myGames.indexOf(game), 1)
				}
			})
		},
		methods: {
			signup(id) {
				if (this.myGames.filter(g => g.id == id)[0]) {
					this.$router.push({name: 'game', params: {game_id: id}})
					return
				}
				queue_channel.push("signup", {id})
					.receive("ok", _ => {
						this.$router.push({name: 'game', params: {game_id: id}})
					})
					.receive("error", e => {
						console.log(e)
					})
			}
		},
		
	}
</script>

<style scoped>
	.game-list {
		border: 0;
		width: 100%;
		td {
		  cursor: pointer;
			padding: 2px
		}
		th {
		  text-align: left;
		}
	}
</style>
