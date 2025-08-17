#ifndef _OPENCOG_THREAD_SAFE_KLD_H
#define _OPENCOG_THREAD_SAFE_KLD_H

#include <mutex>
#include <memory>
#include "KLD.h"

namespace opencog {

/**
 * Thread-safe wrapper for KLDS class
 * 
 * This wrapper provides thread-safe access to KLDS functionality by using
 * a mutex to protect concurrent access. It's designed to resolve the
 * thread safety issue identified in bscores.h where KLDS<contin_t> _klds
 * was marked as mutable but not thread-safe.
 */
template<typename FloatT>
class ThreadSafeKLDS {
private:
    mutable std::mutex _mutex;
    KLDS<FloatT> _klds;

public:
    // Forward constructors to the underlying KLDS
    ThreadSafeKLDS() = default;
    
    template<typename SortedSeq>
    ThreadSafeKLDS(const SortedSeq& p) : _klds(p) {}
    
    ThreadSafeKLDS(const typename KLDS<FloatT>::pdf_t& p_pdf, FloatT p_s = -1) 
        : _klds(p_pdf, p_s) {}

    // Thread-safe wrapper methods
    void set_p_pdf(const typename KLDS<FloatT>::pdf_t& p_counter, FloatT p_s = -1) {
        std::lock_guard<std::mutex> lock(_mutex);
        _klds.set_p_pdf(p_counter, p_s);
    }

    template<typename SortedSeq>
    void set_p(const SortedSeq& p) {
        std::lock_guard<std::mutex> lock(_mutex);
        _klds.set_p(p);
    }

    size_t p_size() const {
        std::lock_guard<std::mutex> lock(_mutex);
        return _klds.p_size();
    }

    size_t p_pdf_size() const {
        std::lock_guard<std::mutex> lock(_mutex);
        return _klds.p_pdf_size();
    }

    FloatT next(const typename KLDS<FloatT>::pdf_t& q_counter, FloatT q_s, 
                FloatT& q_x_pre, typename KLDS<FloatT>::pdf_cit& cit_p, 
                typename KLDS<FloatT>::pdf_cit& cit_q) {
        std::lock_guard<std::mutex> lock(_mutex);
        return _klds.next(q_counter, q_s, q_x_pre, cit_p, cit_q);
    }

    FloatT operator()(const typename KLDS<FloatT>::pdf_t& q_counter) {
        std::lock_guard<std::mutex> lock(_mutex);
        return _klds(q_counter);
    }

    template<typename Out>
    void operator()(const typename KLDS<FloatT>::pdf_t& q_counter, Out out) {
        std::lock_guard<std::mutex> lock(_mutex);
        _klds(q_counter, out);
    }

    // Provide access to the underlying KLDS for advanced usage
    KLDS<FloatT>& get_klds() {
        std::lock_guard<std::mutex> lock(_mutex);
        return _klds;
    }

    const KLDS<FloatT>& get_klds() const {
        std::lock_guard<std::mutex> lock(_mutex);
        return _klds;
    }
};

} // namespace opencog

#endif // _OPENCOG_THREAD_SAFE_KLD_H