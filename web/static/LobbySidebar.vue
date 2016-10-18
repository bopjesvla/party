<template>
	<div class="lobby-sidebar">
		<div class="vertical-tabs">
			<collapse group="sidebar-tabs" label="Join a Game" :active="true">
				<game-list :games="gamesInSignups"></game-list>
			</collapse>
			<collapse group="sidebar-tabs" label="Create a Setup">
				woop
			</collapse>
			<collapse group="sidebar-tabs" label="Find">
				woop
			</collapse>
		</div>
	</div>
</template>

<script type="text/javascript" charset="utf-8">
	import Collapse from './components/Collapse'
	import GameList from './components/GameList'

	export default {
		components: {Collapse, GameList},
		data() {
			return {
				gamesInSignups: [{s: 'Vengeful', p: 7, t: 9}]
			}
		}
	}
</script>

<style type="text/css" media="screen">
	$icon-width: 70px;
	$bg: color(blue shade(80%));
	$active-color: black;

	.lobby-sidebar {
		.vertical-tabs {
			position: absolute;
			background-color: $bg;
			top: 0;
			bottom: 0;
			left: 0;
			right: 0;
			label {
				padding: 5px;
				display: block;
				letter-spacing: 2px;
			}
			.content {
				padding: 5px;
				height: 100%;
			}
		}
	}
</style>
