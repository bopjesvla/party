<template>
	<div class="lobby-sidebar">
		<header>
			<h1>
				rty.party
			</h1>
		</header>
		<div class="sidebar-actions">
			<collapse group="sidebar-tabs" label="Queue" :active="true">
				<game-list :games="gamesInSignups"></game-list>
			</collapse>
			<collapse group="sidebar-tabs" label="New">
				<create-game></create-game>
			</collapse>
			<collapse group="sidebar-tabs" label="Rooms" :active="true">
				<router-link class="room" :to="{name: 'room', params: {name: room.name}}" v-for="room in joinedRooms">{{room.name}}</router-link>
				<form @submit.prevent="$router.push({name: 'room', params: {name: roomInput}}), roomInput = ''">
					<input type="search" v-model=roomInput>
					<button type="submit" class="arrow-before"></button>
				</form>
			</collapse>
		</div>
	</div>
</template>

<script type="text/javascript" charset="utf-8">
	import Collapse from './components/Collapse'
	import GameList from './components/GameList'
	import CreateGame from './components/CreateGame'
	import {queue_channel} from './socket'

	export default {
		components: {Collapse, GameList, CreateGame},
		data() {
			return {
				joinedRooms: [{name: 'lobby'}, {name: 'test'}],
				roomInput: ''
			}
		},
		created() {
			
		}
	}
</script>

<style>
	$icon-width: 70px;
	$bg: brown;
	$active-color: black;

	.lobby-sidebar {
		.sidebar-actions {
			position: absolute;
			top: 60px;
			bottom: 0;
			left: 0;
			right: 0;
			.content {
				padding: 5px;
				height: 100%;
			}
			a.room {
				display: block;
				padding: 2px;
				&.router-link-active {
					font-weight: bold;
				}
			}
			button {
				font-size: .75em;
				vertical-align: bottom;
			}
		}
	}
</style>
