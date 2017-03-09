<template>
  <span class="role-part" @mouseover="onHover" @mouseleave="hover = false">
    {{part}}
    <div v-if="hover && tooltip" class="role-tooltip">
      <h4 class="role-part">{{part}}</h4>
      <div v-html="tooltip"></div>
    </div>
  </span>
</template>

<script type="text/javascript" charset="utf-8">
  export default {
    data() {
      return {hover: false, tooltip: null}
    },
    props: ["part"],
    methods: {
      onHover(e) {
        this.hover = true
        this.$http.get(`/role?role=${this.part}`).then(response => {
          this.tooltip = response.body
        }, e => console.log(e))
      }
    }
  }
</script>

<style>
  .role-part {
    text-transform: capitalize;
  }
  .role-tooltip {
    text-transform: none;
    background: rgba(0,0,0,.9);
    border: 1px solid black;
    color: white;
    bottom: 0;
    right: 0;
    position: fixed;
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
