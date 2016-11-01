<template>
	<div id="home">
		<aside ref="sidebar" v-show="sidebarVisible">
			<router-view name="sidebar"></router-view>
		</aside>
		<button :class="sidebarVisible ? 'icon-close' : 'icon-menu'" @click="sidebarVisible = !sidebarVisible"></button>
		<main :class="{push: sidebarVisible}">
			<!--<h1>party party party party party party party party</h1>-->
			<router-view></router-view>
		</main>
	</div>
</template>

<script>
	import {util} from 'vue'

	let width = w => typeof matchMedia !== 'undefined' && matchMedia(`(min-width: ${w}px)`).matches

	export default {
		data() {
			return {
				sidebarVisible: width(800),
				ie9: util.isIE9,
				error: '',
				user: null
			}
		},
	}
</script>

<style type="text/css" media="screen">
	h1 {
		font-family: 'Comic Sans', 'Comic Sans MS', 'Chalkboard', 'ChalkboardSE-Regular', 'Marker Felt', 'Purisa', 'URW Chancery L', cursive;
	}
	#home {
		&, & input {
			font-size: responsive 14px 16px;
		}
		height: 100%;
		> aside {
			min-width: 320px;
			width: 15%;
			color: white;
			background: black;
			position: absolute;
			top: 0;
			left: 0;
			height: 100%;
		}
		> main {
			position: relative;
			height: 100%;
			min-height: 100%;
			&.push {
				margin-left: 320px;
			}
		}
		> button {
			position: absolute;
		}
	}
</style>
