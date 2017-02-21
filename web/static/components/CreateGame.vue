<template>
	<div>
		<v-select
			@search-change="updateOptions"
			@input="searchSetup"
			placeholder="Name"
			v-model="setup.name"
			:options="options">
			<template slot="no-result"></template>
		</v-select>

		<v-select v-for="r in roles"
			@search-change="updateOptions"
			placeholder="name"
			v-model="setup.name"
			:options="options">
			<template slot="no-result"></template>
		</v-select>

		<button @click="create" type="submit">New Game</button>
	</div>
</template>
<script>
	import {queue_channel} from '../socket'

	export default {
		data() {
			return {
				gameSetup: {},
				mySetup: {
					id: 0,
					name: "Simple",
					roles: [],
					phases: ["day", "night"],
					teams: [{player: 1, team: "m"}, {player: 2, team: "t"}, {player: 3, team: "t"}, {player: 4, team: "t"}]
				},
				showMySetup: true,
				options: []
			}
		},
		watch: {
			$route() {
				if (this.$route.params.game_id) {
					queue_channel.push("setup_info", {game_id: this.$route.params.game_id})
					  .receive("ok", res => {
							this.gameSetup = res.setup
							this.showMySetup = false
						})
				}
			}
		},
		methods: {
			create() {
				queue_channel.push("new:game", {setup_id: 0, speed: 1})
				  .receive("ok", res => this.$router.push(`/game/${res.id}`))
			},
			revert() {
				if (this.$route.params.game_id) {
					queue_channel.push("setup_info", {game: 0, speed: 1})
					  .receive("ok", res => this.$router.push(`/game/${res.id}`))
				}
			},
			updateOptions(search) {
				this.options = [search]
			}
		},
		computed: {
			setup() {
				return this.showMySetup ? this.mySetup : this.gameSetup
			}
		}
	}
</script>
