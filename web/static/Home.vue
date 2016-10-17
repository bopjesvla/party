<template>
	<div id="home">
		<aside ref="sidebar">
			<router-view name="sidebar"></router-view>
		</aside>
		<main>
			<!--<h1>party party party party party party party party</h1>-->
			<router-view></router-view>
		</main>
	</div>
</template>

<script>
</script>

<style type="text/css" media="screen">
	h1 {
		font-family: 'Comic Sans', 'Comic Sans MS', 'Chalkboard', 'ChalkboardSE-Regular', 'Marker Felt', 'Purisa', 'URW Chancery L', cursive;
	}
	aside {
		min-width: 320px;
		width: 15%;
		color: white;
		background: black;
		position: absolute;
		top: 0;
		left: 0;
		height: 100%;
	}
</style>
