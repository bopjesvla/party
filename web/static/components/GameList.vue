<template>
	<table class="game-list">
		<tr v-for="game in games">
			<td class="setup-name">{{game.setup}}</td>
			<td class="players">{{game.count}}/{{game.size}}</td>
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
