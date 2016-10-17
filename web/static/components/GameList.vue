<template>
	<table class="game-list">
		<tr v-for="game in games">
			<td class="setup-name">{{game.s}}</td>
			<td class="players">{{game.p}}/{{game.t}}</td>
		</tr>
	</table>
</template>

<script>
	export default {
		props: {
			games: Array
		}
	}
</script>

<style scoped>
	table {
		border: 0;
		td {
			padding: 2px
		}
	}
</style>
