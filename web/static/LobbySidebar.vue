<template>
	<div class="lobby-sidebar">
		<div class="vertical-tabs">
			<tab group="sidebar-tabs" label="Games" :active="true">
				<game-list :games="gamesInSignups"></game-list>
			</tab>
			<tab group="sidebar-tabs" label="Setups">
				woop
			</tab>
			<tab group="sidebar-tabs" label="Find">
				woop
			</tab>
		</div>
	</div>
</template>

<script type="text/javascript" charset="utf-8">
	import Tab from './components/Tab'
	import GameList from './components/GameList'

	export default {
		components: {Tab, GameList},
		data() {
			return {
				gamesInSignups: [{s: 'Vengeful', p: 7, t: 9}]
			}
		}
	}
</script>

<style type="text/css" media="screen">
	$icon-width: 70px;
	$bg: color(red shade(40%));
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
				display: block;
				width: 70px;
				height: 70px;
				padding: 5px;
				font-style: italic;
			}
			.content {
				top: 0;
				padding: 5px;
				background: $active-color;
				height: 100%;
				left: 71px;
				right: 0;
			}
			.hidden-tab-input:checked + label {
				width: 71px;
				z-index: 5;
				position: relative;
				background-color: $active-color;
			}
		}
	}
</style>
