local month_names = {
    "January", "February", "March", "April", "May", "June", "July", 
    "August", "September", "October", "November", "December"
}

local function make_guard(filename)
    local guard = filename:upper():gsub("[^%w_]", "_")
    return guard
end

local function get_system_time()
    local year = os.date("%Y")
    local month = month_names[tonumber(os.date("%m"))]
    local day = os.date("%d")

    local hour = tonumber(os.date("%H"))
    local minute = os.date("%M")

    local ampm = "am"
    if hour >= 12 then
        ampm = "pm"
        if hour > 12 then 
            hour = hour - 12
        end
    elseif hour == 0 then
        hour = 12
    end

    return string.format("%s %s %s %02d:%02d %s", month, day, year, hour, minute, ampm)
end

-- Autocmd for .h and .hpp files (header guards)
vim.api.nvim_create_autocmd("BufNewFile", {
    pattern = {"*.h", "*.hpp"},
    callback = function()
        local filename = vim.fn.expand("%:t")
        local guard = make_guard(filename)
        local date = get_system_time()
        local username = "Justin Lewis"

        vim.api.nvim_buf_set_lines(0, 0, 0, false, {
            "#if !defined(" .. guard .. ")",
            "/* ========================================================================",
            "   $File: " .. filename.. " $",
            "   $Date: " .. date .. " $",
            "   $Revision: $", 
            "   $Creator: " .. username .. " $",
            "   ======================================================================== */",
            "",
            "#define " .. guard,
            "",
            "#endif // " .. guard
        })
    end
})

-- Autocmd for .c and .cpp files (metadata comment block)
vim.api.nvim_create_autocmd("BufNewFile", {
    pattern = {"*.c", "*.cpp", "*.odin, *.zig, *.rs"},
    callback = function()
        local filename = vim.fn.expand("%:t")
        local date = get_system_time()
        local username = "Justin Lewis"

        vim.api.nvim_buf_set_lines(0, 0, 0, false, {
            "/* ========================================================================",
            "   $File: " .. filename.. " $",
            "   $Date: " .. date .. " $",
            "   $Revision: $", 
            "   $Creator: " .. username .. " $",
            "   ======================================================================== */",
            "",
        })
    end
})
