<template>
	<v-select
		@search-change="search = $event"
		placeholder="Team"
		:value="value"
    @input="$emit('input', $event)"
		:allow-empty="false"
		:options="[search.toLowerCase(), 'town', 'mafia']">
	</v-select>
</template>
<script>
	export default {
		data() {
			return {
				search: ''
			}
		},
    props: ["value"]
	}
</script>
<style>
	.team {
		input, .multiselect__option , .multiselect__tag {
			text-transform: capitalize !important;
		}
	}
</style>
