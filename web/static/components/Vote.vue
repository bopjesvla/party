<template>
  <span>{{text}}</span>
</template>

<script>
  export default {
    props: ["vote"],
    computed: {
      text() {
        let targets = this.vote.opt.map(t => {
					if (typeof t == "number") {
						return this.vote.slotName(t)
					}
					else {
						return t
					}
				})
				return `${this.vote.act} ${this.vote.opt.join(", ")}`
      }
    }
  }
</script>

<style lang="css">

</style>
