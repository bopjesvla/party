<template>
  <span class="role-part" @mouseover="hover = true" @mouseleave="hover = false">
    {{part}}
  </span>
  <v-if="hover && tooltip" class="role-part">
    {{{tooltip}}}
  </span>
</template>

<script type="text/javascript" charset="utf-8">
  export default {
    data: {hover: false, tooltip: null},
    props: ["part"],
    methods: {
      showTooltip(e) {
        this.$http.get(`roles/${this.part}`).then(response => {
          this.tooltip = response.body
        }, e => console.log(e))
      }
    }
  }
</script>

<style>
  .role {
    text-transform: capitalize;
  }
</style>
