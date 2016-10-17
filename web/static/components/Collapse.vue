<template>
	<section class="tab">
		<input ref="input" class="hidden-tab-input" type="checkbox" :value="label" :id="id" :checked="!collapsed">
		<label :for="id">{{label}}</label>
		<div class="content">
			<slot></slot>
		</div>
	</section>
</template>

<script type="text/javascript" charset="utf-8">
	export default {
		props: {
				id: {
					type: String,
						default() { return `tab-${this._uid}` }
				},
				label: String,
				group: {
					type: String,
					required: true
				},
				collapsed: Boolean
		},
		computed: {
			activeTab: {
				get() {
					return this.$refs.input.value
				},
				set(name) {
					this.$refs.input.value = name
				},
				cache: false
			}
		}
	}
</script>

<style>
	.tab {
		.hidden-tab-input {
			display: none;
			&:checked + label {
				+ .content {
					display: block;
				}
			}
		}
		label {
			cursor: pointer;
		}
		.content {
			display: none;
			position: absolute
		}
	}
</style>
