<template>
	<div>
		<v-select
			@search-change="search = $event"
			@input="searchSetup"
			placeholder="Name"
			v-model="setup.name"
			:close-on-select="false"
			:options="[search].concat(nameOptions)">
		</v-select>
		Size: <input type="number" v-model.number="setup.size" min=1 max=25>

		<div class="roles">
			<h3>Roles</h3>
			<div class="role" v-for="r in setup.roles">
				<v-select
					placeholder="Type"
					v-model="r.type"
					:options="typeOptions">
				</v-select>
				<input type="text" v-if="r.type == 'team'" placeholder="team" v-model="r.str">
				<v-select v-if="r.type == 'player'"
					placeholder="Player"
					v-model="r.nr"
					:options="players">
				</v-select>
				<v-select
					@search-change="getRoles"
					placeholder="Mods"
					v-model="r.mods"
					:multiple="true"
					:options="modOptions">
				</v-select>
				<v-select
					@search-change="getRoles"
					placeholder="Role"
					v-model="r.role"
					:options="roleOptions">
				</v-select>
			</div>
			<button style="display : inline;" @click="setup.roles.push({role: null, mods: null, type: 'player', nr: 1, str: null})">+</button>
		</div>
		<div class="teams">
			<h3>Teams</h3>
			<div class="team" v-for="r in setup.teams">
				{{r.player}}
				<input type="text" v-model="r.team" placeholder="team">
			</div>
		</div>

		<button @click="createGame">New Game</button>
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
					size: 4,
					name: "Simple",
					roles: [],
					phases: ["day", "night"],
					teams: [{player: 1, team: "mafia"}, {player: 2, team: "town"}, {player: 3, team: "town"}, {player: 4, team: "town"}]
				},
				showMySetup: true,
				nameOptions: [],
				roleOptions: [],
				modOptions: [],
				typeOptions: ["player", "team", "global"],
				search: ''
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
			},
			"setup.size": function() {
				let {size, teams} = this.setup
				if (!size) {
					return
				}
				while (teams.length < size) {
					teams.push({player: teams.length + 1, team: "town"})
				}
				while (teams.length > size) {
					teams.pop()
				}
			},
			search() {
				if (this.search.length == 3) {
					queue_channel.push("list:setups", {search: this.search})
						.receive("ok", res => this.nameOptions = res.setups)
				}
			}
		},
		methods: {
			createGame() {
				queue_channel.push("new:game", {setup_id: 0, speed: 1})
				  .receive("ok", res => this.$router.push(`/game/${res.id}`))
			},
			revert() {
				if (this.$route.params.game_id) {
					queue_channel.push("setup_info", {game_id: 0})
					  .receive("ok", res => {
							this.gameSetup = res.setup
						})
				}
			},
			getRoles() {
				if (!this.roleOptions.length) {
					queue_channel.push("role_info", {game: 0, speed: 1})
						.receive("ok", res => {
							this.roleOptions = res.roles
							this.modOptions = res.mods
						})
				}
			},
			searchSetup() {
				queue_channel.push("setup_info", {name: setup.name})
					.receive("ok", res => this.mySetup = res.setup)
			}
		},
		computed: {
			setup() {
				return this.showMySetup ? this.mySetup : this.gameSetup
			},
			players() {
				return Array.apply(null, {length: this.setup.size + 1})
					.map(Number.call, Number)
			}
		}
	}
</script>
<style>
	.roles {
		margin-top: 10px;
	}
</style>
