<template>
  <span class="role">
    <role-part :part="m" v-for="m in role.mods"></role-part>
    <role-part :part="role.role"></role-part>
  </span>
</template>

<script type="text/javascript" charset="utf-8">
	import RolePart from './RolePart'
  export default {
    props: ["role", "colored"],
    components: {RolePart}
  }
</script>

<style>
  .role {
    text-transform: capitalize;
  }
</style>
