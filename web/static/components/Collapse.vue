<template>
	<section class="tab">
		<input ref="input" class="hidden-tab-input" type="checkbox" :value="label" :id="id" :checked="collapsed">
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
		}
	}
</script>

<style>
	.tab {
		.hidden-tab-input {
			display: none;
			&:checked + label {
				+ .content {
					display: none;
				}
			}
		}
		label {
			cursor: pointer;
			user-select: none;
		}
	}
</style>
