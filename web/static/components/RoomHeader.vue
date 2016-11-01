<template>
	<header>
		<h3 :name=name></h3>
	</header>
</template>
<script>
	export default {
		props: {
			name: String
		}
	}
</script>
