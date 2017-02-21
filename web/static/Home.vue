<template>
	<div id="home">
		<aside ref="sidebar" v-show="sidebarVisible">
			<router-view name="sidebar"></router-view>
		</aside>
		<main :class="{push: sidebarVisible}">
			<a class="arrow" :class="sidebarVisible ? 'icon-close' : 'icon-menu'" @click="sidebarVisible = !sidebarVisible"></a>
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
		text-align: center;
	}
	#home {
		&, & input.inline {
			font-size: responsive 14px 16px;
		}
		height: 100%;
		> aside {
			line-height: normal;
			min-width: 320px;
			width: 15%;
			color: white;
			position: absolute;
			top: 0;
			left: 0;
			height: 100%;
			z-index: 2;
			overflow-y: auto;
			input {
				width: 90px;
			}
			input, button {
				color: white
			}
			button {
				background-color: transparent;
				padding: 0;
			}
		}
		> main {
			position: relative;
			background-color: #fff;
			height: 100%;
			min-height: 100%;
			.arrow {
				position: absolute;
				top: 10px;
				z-index: 3;
				color: #000;
				&:before {
					position: absolute;
					top: -15px;
					left: -15px;
					right: -15px;
					bottom: -15px;
					content: '';
				}
			}
			&.push {
				margin-left: 320px;
				.arrow {
					color: #fff;
					position: absolute;
					left: -1em;
					border-color: white;
					transform: rotate(-135deg);
				}
			}
		}
	}
</style>
