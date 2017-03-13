<template>
  <span class="role">
    <description :name="m" type="modifier" v-for="m in role.mods"></description>
    <description :name="role.role" type="role"></description>
  </span>
</template>

<script type="text/javascript" charset="utf-8">
	import Description from './Description'
  export default {
    props: ["role", "colored"],
    components: {Description}
  }
</script>

<style>
  .role {
    text-transform: capitalize;
  }
</style>
