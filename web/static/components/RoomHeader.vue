<template>
	<header class="room-header">
		<h3>{{name}}</h3>
	</header>
</template>
<script>
	export default {
		props: {
			name: String
		}
	}
</script>
<style>
	.room-header {
		position: absolute;
		top: 0;
		left: 0;
		padding-left: 20px;
	}
</style>
