<template>
	<span class="role">
		{{role.mods.join(" ")}} {{role.role}}
	</span>
</template>

<script type="text/javascript" charset="utf-8">
	export default {
		props: ["role"]
	}
</script>

<style>
	.role {
		text-transform: capitalize;
	}
</style>
