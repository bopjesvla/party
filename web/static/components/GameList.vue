<template>
	<table class="game-list">
		<tr v-for="game in games" @click="signup(game.id)">
			<td class="setup-name">{{game.setup}}</td>
			<td class="players">{{game.count}}/{{game.size}}</td>
		</tr>
	</table>
</template>

<script>
	import {queue_channel} from "../socket"

	export default {
		props: {
			games: Array
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
