<template>
	<div v-if="setup">
		<v-select
			@search-change="search = $event"
			@input="searchSetup"
			placeholder="Name"
			v-model="setup.name"
			:options="[search].concat(nameOptions)">
		</v-select>
		<span v-if="error">{{error}}</span>
		Size: <input type="number" v-model.number="setup.size" min=1 max=25>

		<div class="roles">
			<h3>Roles</h3>
			<div class="role" v-for="r in setup.roles">
				<v-select
					placeholder="Type"
					v-model="r.type"
					:options="typeOptions">
				</v-select>
				<team-input v-if="r.type == 'team'" v-model="r.str"></team-input>
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
				Player {{r.player}}
				<team-input v-model="r.team"></team-input>
			</div>
		</div>

		<button @click="createSetup">Create Setup</button> <button @click="createGame">Create Game</button>
	</div>
</template>
<script>
	import {queue_channel} from '../socket'
	import TeamInput from './TeamInput.vue'

	export default {
		data() {
			return {
				setup: null,
				alteredSetup: false,
				nameOptions: [],
				roleOptions: [],
				modOptions: [],
				typeOptions: ["player", "team", "global"],
				search: '',
				error: null
			}
		},
		created() {
			if (this.$route.params.game_id) {
				queue_channel.push("setup_info", {game_id: this.$route.params.game_id})
					.receive("ok", res => {
						this.setSetup(res.setup)
					})
			}
			else {
				queue_channel.push("setup_info", {name: "Simple"})
					.receive("ok", res => {
						this.setSetup(res.setup)
					})
			}
		},
		watch: {
			$route() {
				if (this.$route.params.game_id) {
					queue_channel.push("setup_info", {game_id: this.$route.params.game_id})
					  .receive("ok", res => {
							this.setSetup(res.setup)
						})
				}
			},
			setup() {
				this.alteredSetup = true
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
				queue_channel.push("new:game", {setup_id: this.setup.id, speed: 1})
				  .receive("ok", res => {
				  	this.$router.push(`/game/${res.id}`)
				  })
			},
			createSetup() {
				queue_channel.push("new:setup", {setup: this.setup})
				  .receive("ok", res => {
						this.setup.id = res.id
						this.alteredSetup = false
					})
					.receive("error", e => this.error = e.errors)
			},
			revert() {
				if (this.$route.params.game_id) {
					queue_channel.push("setup_info", {game_id: 0})
					  .receive("ok", res => {
							this.setup = res.setup
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
				queue_channel.push("setup_info", {name: this.setup.name})
					.receive("ok", res => this.setup = res.setup)
			},
			setSetup(s) {
				this.setup = s
				this.alteredSetup = false
			}
		},
		computed: {
			players() {
				return Array.apply(null, {length: this.setup.size + 1})
					.map(Number.call, Number)
			}
		},
		components: {TeamInput}
	}
</script>
<style>
	.roles {
		margin-top: 10px;
		input, .multiselect__option , .multiselect__tag {
			text-transform: capitalize !important;
		}
		.role {
			margin-bottom: 10px
		}
	}
</style>
