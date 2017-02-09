<template>
	<section class="collapse">
		<input ref="input" class="hidden-tab-input" type="checkbox" :value="label" :id="id" :checked="collapsed">
		<label :for="id" class="arrow-before">{{label}}</label>
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
	.collapse {
		.hidden-tab-input {
			display: none;
			&:checked + label {
				&:before {
					transform: rotate(135deg);
				}
				+ .content {
					display: none;
				}
			}
		}
		> label {
			display: block;
			cursor: pointer;
			user-select: none;
			padding: 5px;
			background: rgba(0, 0, 0, 0.2);
			&:before {
				position: relative;
				top: -3px;
				margin-right: 5px;
				transform: rotate(-45deg);
				font-size: .3em;
			}
		}
	}
</style>
