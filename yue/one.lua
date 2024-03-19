local _module_0 = { }
local p, to_lua
do
	local _obj_0 = require("yue")
	p, to_lua = _obj_0.p, _obj_0.to_lua
end
local inventory = {
	equipment = {
		"sword",
		"shield"
	},
	items = {
		{
			name = "potion",
			count = 10
		},
		{
			name = "bread",
			count = 3
		}
	}
}
print(reduce(filter(map({
	1,
	2,
	3
}, function(x)
	return x * 2
end), function(x)
	return x > 4
end), 0, function(a, b)
	return a + b
end))
local apple = setmetatable({
	size = 15,
}, {
	__index = {
		color = 0x00ffff
	}
})
if (getmetatable(apple) ~= nil) then
	p(apple.color, getmetatable(apple).__index)
end
local _ud83c_udf1b = "月之脚本"
_module_0["🌛"] = _ud83c_udf1b
return _module_0
