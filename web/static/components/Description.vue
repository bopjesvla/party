<template>
  <span class="description" @mouseover="onHover" @mouseleave="hover = false">
    {{name}}
    <div v-if="hover && tooltip" class="tooltip">
      <h4 class="name">{{name}}</h4>
      <div v-html="tooltip"></div>
    </div>
  </span>
</template>

<script type="text/javascript" charset="utf-8">
  export default {
    data() {
      return {hover: false, tooltip: null}
    },
    props: ["name", "type"],
    methods: {
      onHover(e) {
        this.hover = true
        this.$http.get(`/description/${this.type}/${this.name}`).then(response => {
          this.tooltip = response.body
        }, e => console.log(e))
      }
    }
  }
</script>

<style>
  .description .name {
    text-transform: capitalize;
  }
  .tooltip {
    text-transform: none;
    background: rgba(0,0,0,.9);
    border: 1px solid black;
    color: white;
    bottom: 0;
    right: 0;
    position: fixed;
    z-index: 99;
    padding: 20px;
    font-size: .9em;
    cursor: default;
    h4 {
      margin-top: 0;
    }
    li:before {
      content: "- ";
      font-weight: bold;
    }
  }
</style>
