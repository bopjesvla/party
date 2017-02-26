<template>
	<table class="game-list">
		<tr v-for="game in gamesInSignups" @click="signup(game.id)">
			<td class="setup-name">{{game.setup}}</td>
			<td class="players">{{game.count}}/{{game.size}}</td>
		</tr>
	</table>
</template>

<script>
	import {queue_channel, user_channel} from "../socket"

	export default {
		data() {
			return {
				gamesInSignups: []
			}
		},
		created() {
			queue_channel.push("list:games", {})
				.receive("ok", d => this.gamesInSignups = d.games)
			// user_channel.push("list:games", {})
			// 	.receive("ok", d => this.gamesInSignups = d.games)
			queue_channel.on("count", msg => {
				let game = this.gamesInSignups.filter(g => g.id == msg.id)[0]
				if (game) {
				  game.count = msg.count
				}
			})
			queue_channel.on("new:game", msg => {
				this.gamesInSignups.unshift(msg)
			})
		},
		methods: {
			signup(id) {
				queue_channel.push("signup", {id})
					.receive("ok", _ => {
						this.$router.push({name: 'game', params: {game_id: id}})
					})
					.receive("error", e => {
						console.log(e)
					})
			}
		}
	}
</script>

<style scoped>
	.game-list {
		border: 0;
		width: 100%;
		td {
			padding: 2px
		}
	}
</style>
