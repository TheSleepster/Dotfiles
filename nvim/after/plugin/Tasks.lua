

function Sleepster_Build()
    local cwd = vim.fn.getcwd()
    local filepath = cwd .. "/build.bat"
    local current_win = vim.api.nvim_get_current_win()
    local current_buf = vim.api.nvim_get_current_buf()
    local wins = vim.api.nvim_tabpage_list_wins(0)

    vim.cmd("wa")
    local target_win
    if #wins < 2 then
        vim.cmd("vsplit")  -- Split if only one window exists
        wins = vim.api.nvim_tabpage_list_wins(0)
        for _, win in ipairs(wins) do
            if win ~= current_win then
                target_win = win
                break
            end
        end
    else
        for _, win in ipairs(wins) do
            if win ~= current_win then
                target_win = win
                break
            end
        end
    end

    vim.api.nvim_set_current_win(target_win)
    vim.cmd('terminal "' .. filepath .. '"')
    local term_buf = vim.api.nvim_get_current_buf()

    vim.api.nvim_create_autocmd("TermClose", {
        buffer = term_buf,
        once = true,  -- Run only once for this terminal job
        callback = function()
            local lines = vim.api.nvim_buf_get_lines(term_buf, 0, -1, false)
            vim.opt.errorformat = [[%f(%l): %t%*[^:]: %m]]

            vim.fn.setqflist({}, ' ', { title = 'Build Output', lines = lines })
        end,
    })

    -- Restore original window and buffer
    vim.api.nvim_set_current_win(current_win)
    vim.api.nvim_set_current_buf(current_buf)
end

local last_qf_idx = 0
function Sleepster_NextBuildError()
    local qflist = vim.fn.getqflist()
    if #qflist == 0 then
        return
    end

    if last_qf_idx >= #qflist then
        return
    end

    if last_qf_idx == 0 then
        last_qf_idx = 1
        vim.cmd("cfirst")
        vim.cmd("cnext")
    elseif last_qf_idx < #qflist then
        last_qf_idx = last_qf_idx + 1
        vim.cmd("cnext")
    else
        return
    end

    local qf_idx = vim.fn.getqflist({ idx = 1 }).idx
    local item = qflist[qf_idx]

    if item and item.filename then
        vim.cmd("edit " .. item.filename)
        vim.api.nvim_win_set_cursor(0, { item.lnum, 0 })
    end
end

function Sleepster_Run()
    local cwd = vim.fn.getcwd()
    local rundir = cwd .. "/../build/"
    local exe_name = nil

    local current_win = vim.api.nvim_get_current_win()
    local win_count = #vim.api.nvim_tabpage_list_wins(0)
    local current_buf = vim.api.nvim_get_current_buf()

    local files = vim.fn.glob(rundir .. "*.exe", 0, 1)
    if #files == 0 then
        print("Your Money is NOT like Lizzo, your pockets are anorexic")
        return
    end
    exe_name = files[1]

    if win_count < 2 then
        vim.cmd('vsplit')
    else
        local wins = vim.api.nvim_tabpage_list_wins(0)
        for _, win in ipairs(wins) do
            if win ~= current_win then
                vim.api.nvim_set_current_win(win)
                break
            end
        end
    end


    vim.cmd('terminal"' .. exe_name .. '"')
    vim.api.nvim_set_current_win(current_win)
    vim.api.nvim_set_current_buf(current_buf)
end

-- FILE SWAP
function Sleepster_GetCorrespondingFile()
    local CurrentFile = vim.fn.expand("%:t")
    local CurrentDir  = vim.fn.expand("%:p:h")
    local CurrentFileExtension = vim.fn.expand("%:e")

    local CorrespondingFile = ""

    if CurrentFileExtension == "cpp" or CurrentFileExtension == "c" then
        -- Look for .h or .hpp file
        local HeaderFile = CurrentFile:gsub("%.cpp", ".h"):gsub("%.c", ".h")
        local HeaderFileHpp = CurrentFile:gsub("%.cpp", ".hpp"):gsub("%.c", ".hpp")

        if vim.fn.filereadable(CurrentDir .. "/" .. HeaderFile) == 1 then
            CorrespondingFile = HeaderFile
        elseif vim.fn.filereadable(CurrentDir .. "/" .. HeaderFileHpp) == 1 then
            CorrespondingFile = HeaderFileHpp
        end
    elseif CurrentFileExtension == "h" or CurrentFileExtension == "hpp" then
        -- Look for .cpp or .c file
        local SourceFileCpp = CurrentFile:gsub("%.h", ".cpp"):gsub("%.hpp", ".cpp")
        local SourceFileC = CurrentFile:gsub("%.h", ".c"):gsub("%.hpp", ".c")

        if vim.fn.filereadable(CurrentDir .. "/" .. SourceFileCpp) == 1 then
            CorrespondingFile = SourceFileCpp
        elseif vim.fn.filereadable(CurrentDir .. "/" .. SourceFileC) == 1 then
            CorrespondingFile = SourceFileC
        end
    else
        vim.api.nvim_err_writeln("Unsupported file type.")
        return nil
    end

    return CorrespondingFile
end


function Sleepster_DisplayCorrespondingFileSameBuffer()
    local File = Sleepster_GetCorrespondingFile()

    vim.api.nvim_command("edit " .. File)
end


function Sleepster_DisplayCorrespondingFileOppositeBuffer()
    local File = Sleepster_GetCorrespondingFile()

    vim.api.nvim_command("wincmd w")
    vim.api.nvim_command("edit " .. File)
end

vim.api.nvim_create_user_command('Build', Sleepster_Build, {})
vim.api.nvim_create_user_command('DisplaySB', Sleepster_DisplayCorrespondingFileSameBuffer, {})
vim.api.nvim_create_user_command('DisplayOB', Sleepster_DisplayCorrespondingFileOppositeBuffer, {})
vim.api.nvim_create_user_command('Run', Sleepster_Run, {})
vim.api.nvim_create_user_command('NextError', Sleepster_NextBuildError, {})

function CustomHighlight()
    vim.fn.matchadd("DiagnosticError", "// TODO")
    vim.fn.matchadd("DiagnosticInfo", "// NOTE")
    vim.fn.matchadd("DiagnosticWarn", "// WARNING")
    vim.fn.matchadd("DiagnosticWarn", "Sleepster")
    vim.fn.matchadd("DiagnosticWarn", "// HACK")
    vim.fn.matchadd("DiagnosticWarn", "// WARN")
end

vim.api.nvim_create_autocmd({"BufEnter", "BufWinEnter"}, {callback = CustomHighlight})
