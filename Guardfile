def run(f)
  puts "\033[93m#{Time.now}: #{f}\033[0m"
  success = system 'cargo build'
  puts (
    if success
      "\033[92mOK\033[0m\n\n"
    else
      "\033[91mFAIL\033[0m\n\n"
    end
  )
  success
end

guard :shell do
  watch %r[^.+\.rs?$] do |m|
    run m[0]
  end
end
