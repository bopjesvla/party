<template>
	<span class="team" :class="'team-' + team">
		{{team}}
	</span>
</template>

<script type="text/javascript" charset="utf-8">
	export default {
		props: ["team"]
	}
</script>

<style>
	.team {
		text-transform: capitalize;
	}
	.team-mafia {
		text-shadow: 0 0 1px solid red;
	}
</style>
